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
bayesopt_parego = function(instance, acq_function = NULL, acq_optimizer = NULL, q = 1, s = 100, rho = 0.05) {
  assert_r6(instance, "OptimInstanceMultiCrit")
  if (is.null(acq_function)) {
    surrogate = default_surrogate(instance, n_objectives = 1L)
    acq_function = AcqFunctionEI$new(surrogate = surrogate)
  }
  if (is.null(acq_optimizer)) {
    acq_optimizer = default_acqoptimizer(instance)
  }
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_int(q, lower = 1)

  eval_initial_design(instance)
  archive = instance$archive
  dummy_codomain = ParamSet$new(list(ParamDbl$new("y_scal", tags = "minimize")))
  d = archive$codomain$length

  # calculate all possible weights (lambdas) for given s parameter
  comb_with_sum = function(n, d) {
    fun = function(n, d) {
      if (d == 1L)
        list(n)
      else
        unlist(lapply(0:n, function(i) Map(c, i, fun(n - i, d - 1L))), recursive = FALSE)
    }
    matrix(unlist(fun(n, d)), ncol = d, byrow = TRUE)
  }
  lambdas = comb_with_sum(s, d) / s
  q_sec = seq_len(q)

  repeat {
    xydt = archive$data
    xdt = xydt[, archive$cols_x, with = FALSE]
    ydt = xydt[, archive$cols_y, with = FALSE]

    ydt = Map("*", ydt, mult_max_to_min(archive$codomain))
    ydt = Map(function(y) (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE)), ydt)  # scale y to [0, 1]

    # Temp ParEGO Archive
    dummy_archive = archive$clone(deep = TRUE)
    dummy_archive$codomain = dummy_codomain

    xdt = map(q_sec, function(i) {

      # scalarize y
      lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
      mult = Map('*', ydt, lambda)
      y_scal = do.call('+', mult)
      y_scal = do.call(pmax, mult) + rho * y_scal  # augmented Tchebycheff function
      set(dummy_archive$data, j = "y_scal", value = y_scal)

      # opt surrogate
      # manual fix: # FIXME: Write ParEGO Infill Crit?
      tryCatch({
        acq_function$setup(dummy_archive)
        acq_function$surrogate$setup(xydt = archive_xy(dummy_archive), y_cols = dummy_archive$cols_y)  # update surrogate model with new data
        acq_function$update(dummy_archive)
        acq_optimizer$optimize(acq_function)
      }, leads_to_exploration_error = function(leads_to_exploration_error_condition) {
        leads_to_exploration_error_condition
      })
    })

    error_ids = which(map_lgl(xdt, function(x) "leads_to_exploration_error" %in% class(x)))
    for (i in error_ids) {
      lg$info("Proposing a randomly sampled point")  # FIXME: logging?
      xdt[[i]] = SamplerUnif$new(instance$search_space)$sample(1L)$data  # FIXME: also think about augmented lhs
    }
    xdt = rbindlist(xdt, fill = TRUE)

    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(instance$archive)) break
  }

  return(instance$archive)
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

  # Defaults work
  bayesopt_parego(instance, q = 2)

  archdata = instance$archive$data
  library(ggplot2)
  g = ggplot(archdata, aes_string(x = "y1", y = "y2", color = "batch_nr"))
  g + geom_point()
}

