objective = ObjectiveRFun$new(
  fun = function(xs) list(y = sum(unlist(xs)^2)),
  domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
  id = "test"
)

terminator = trm("evals", n_evals = 20)

instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = terminator
)

archive = instance$archive
domain = instance$search_space

surrogate = SurrogateLearner$new(learner = lrn("regr.km"), archive = archive)
acq_function = AcqFunctionEI$new(surrogate = surrogate, domain = domain)
optimizer = opt("random_search", batch_size = 1000)
acq_optimizer = AcqOptimizer$new(optimizer = optimizer, trm("evals", n_evals = 1000), acq_function = acq_function)


d = instance$objective$ydim
random = generate_design_random(domain, n = 4L * d)$data
instance$eval_batch(design)

repeat {
  xdt = tryCatch({
    surrogate$update()
    acq_function$update()
    acq_optimizer$optimize()
  }, mbo_error = function(mbo_error_condition) {
    SamplerUnif$new(domain)$sample(1L)$data
  })

  instance$eval_batch(xdt)
  if (instance$is_terminated) break
}


bayesopt_soo = function(objective, terminator, init_design_size, learner, acq_id, acq_params, acq_optimizer, acq_terminator) {
}

