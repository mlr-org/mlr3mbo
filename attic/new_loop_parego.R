objective = ObjectiveRFun$new(
  fun = function(xs) list(y1 = sum(unlist(xs)^2), y2 = 5),
  domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
  id = "test"
)

terminator = trm("evals", n_evals = 20)

instance = OptimInstanceMultiCrit$new(
  objective = objective,
  terminator = terminator
)

archive = instance$archive
domain = instance$search_space

surrogate = SurrogateLearner$new(learner = lrn("regr.km"), archive = archive, y_col = "y_scal")
acq_function = AcqFunctionEI$new(surrogate = surrogate, domain = domain)
optimizer = opt("random_search", batch_size = 1000)
acq_optimizer = AcqOptimizer$new(optimizer = optimizer, trm("evals", n_evals = 1000), acq_function = acq_function)


d = instance$objective$ydim
random = generate_design_random(domain, n = 4L * d)$data
instance$eval_batch(design)

repeat {
  xdt = tryCatch({
    dd = archive$data
    dy = as.matrix(dd[, archive$cols_y])
    lambda = lambdas[sample(seq_len(nrow(lambdas)), 1L), drop = FALSE]
    y_scal = dy %*% t(lambda)
    dd[, y_scal := y_scal]
    surrogate$update()
    acq_function$update()
    acq_optimizer$optimize()
  }, mbo_error = function(mbo_error_condition) {
    SamplerUnif$new(domain)$sample(1L)$data
  })

  instance$eval_batch(xdt)
  if (instance$is_terminated) break
}

###########################################################################################################################


bayesopt_parego = function(instance, acq_function = NULL, acq_optimizer = NULL, q = 1, s = 100, rho = 0.05) {
  assert_r6(instance, "OptimInstanceMultiCrit")
  if (is.null(acq_function)) {
    surrogate = default_surrogate(instance, n_objectives = 1L)
    acq_function = AcqFunctionEI$new(surrogate = surrogate)
  }
  if (is.null(acq_optimizer)) {
    acq_optimizer = default_acqopt(instance)
  }
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_int(q, lower = 1)

  eval_initial_design(instance)
  archive = instance$archive
  dummy_codomain = ParamSet$new(list(ParamDbl$new("y_scal", tags = "minimize")))
  d = archive$codomain$length

  # FIXME: document
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
        acq_function$update_surrogate(dummy_archive)
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



bayesopt_soo = function(objective, terminator, init_design_size, learner, acq_id, acq_params, acq_optimizer, acq_terminator) {
}

