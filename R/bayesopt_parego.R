# #' @title Multi-Objective ParEGO
# #'
# #' @description
# #' Function that executes a ParEGO multi-objective Bayesian optimization.
# #' @template param_instance
# #' @template param_acq_function
# #' @template param_acq_optimizer
# #' @param q (`int(1)`)\cr
# #'   Batch size of proposal
# #' @param s (`int(1)`)\cr
# #'   ?
# #' @param rho (`numeric(1)`)\cr
# #'   ?
# #' @return [bbotk::Archive]
# #' @export
bayesopt_parego = function(instance, acq_function, acq_optimizer, q = 1, s = 100, rho = 0.05) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_int(q, lower = 1)
  assert_int(s, lower = 1)  # FIXME: s must be larger than k (number of objectives)?
  assert_double(rho, lower = 0, upper = 1)


  archive = instance$archive
  #FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = generate_design_lhs(instance$search_space, 4 * instance$search_space$length)$data
    instance$eval_batch(design)
  }

  # ParEGO Archive
  dummy_codomain = ParamSet$new(list(ParamDbl$new("y_scal", tags = "minimize")))
  dummy_archive = archive$clone(deep = TRUE)
  dummy_archive$codomain = dummy_codomain

  # manual fix: #FIXME: Write ParEGO Infill Crit?
  acq_function$setup(dummy_archive)

  k = archive$codomain$length
  q_seq = seq_len(q)
  lambdas = combWithSum(s, k) / s
  n_lambdas = nrow(lambdas)

  repeat {
    xydt = archive$data
    xdt = xydt[, archive$cols_x, with = FALSE]
    ydt = xydt[, archive$cols_y, with = FALSE]

    # normalize objective values to [0, 1]
    ydt[, (archive$cols_y) := lapply(.SD, function(x) (x - min(x)) / diff(range(x))), .SDcols = (archive$cols_y)]

    ydt = Map("*", ydt, mult_max_to_min(archive$codomain))

    # FIXME: qParEGO should be done similarly to https://github.com/mlr-org/mlrMBO/blob/247e993af210b7c6ebc21f1557cabe4560ec3369/R/makeTasksParEGO.R#L71
    xdt = map(q_seq, function(i) {
      tryCatch({
        lambda = lambdas[sample(n_lambdas, size = 1, replace = FALSE), ]

        mult = Map("*", ydt, lambda)
        y_scal = rho * do.call("+", mult)
        setDT(mult)
        mult[, max := max(.SD), by = seq_len(nrow(mult))]
        y_scal = mult$max + y_scal
        set(dummy_archive$data, j = "y_scal", value = y_scal)

        acq_function$surrogate$setup(xydt = archive_xy(dummy_archive), y_cols = dummy_archive$cols_y)  # update surrogate model with new data
        acq_function$update(dummy_archive)
        acq_optimizer$optimize(acq_function)
      }, leads_to_exploration_error = function(leads_to_exploration_error_condition) {
        leads_to_exploration_error_condition
      })
    })
    id_missing = which(map_lgl(xdt, function(x) "leads_to_exploration_error" %in% class(x)))
    for (i in id_missing) {
      lg$info("Proposing a randomly sampled point")  # FIXME: logging?
      xdt[[i]] = SamplerUnif$new(instance$search_space)$sample(1L)$data
    }
    xdt = rbindlist(xdt, fill = TRUE)

    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(instance$archive)) break
  }

  return(instance$archive)
}


# https://github.com/mlr-org/mlrMBO/blob/247e993af210b7c6ebc21f1557cabe4560ec3369/R/utils.R#L17
# for Parego: calculate all integer vectors of length k with sum n
combWithSum = function(n, k) {
  fun = function(n, k) {
    if (k == 1L)
      list(n)
    else
      unlist(lapply(0:n, function(i) Map(c, i, fun(n - i, k - 1L))), recursive = FALSE)
  }
  matrix(unlist(fun(n, k)), ncol = k, byrow = TRUE)
}

if (FALSE) {
  set.seed(1)
  devtools::load_all()
  library(bbotk)
  library(paradox)
  library(mlr3learners)

  FUN_2D_2D = function(xs) {
    list(y1 = xs[[1]]^2, y2 = -xs[[2]]^2)
  }
  PS_2D = ParamSet$new(list(
    ParamDbl$new("x1", lower = -1, upper = 1),
    ParamDbl$new("x2", lower = -1, upper = 1)
  ))
  FUN_2D_2D_CODOMAIN = ParamSet$new(list(
    ParamDbl$new("y1", tags = "minimize"),
    ParamDbl$new("y2", tags = "maximize")
  ))
  obfun = ObjectiveRFun$new(fun = FUN_2D_2D, domain = PS_2D,
    codomain = FUN_2D_2D_CODOMAIN, properties = "multi-crit")

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  acq_function = AcqFunctionEI$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000))

  bayesopt_parego(instance, acq_function, acq_optimizer, q = 2)

  archdata = instance$archive$data
  library(ggplot2)
  g = ggplot(archdata, aes_string(x = "y1", y = "y2", color = "batch_nr"))
  g + geom_point()
}

