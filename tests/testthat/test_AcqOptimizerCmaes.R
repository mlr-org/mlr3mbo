test_that("AcqOptimizerCmaes works", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxEvals = 100L)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)
  # expect_list(acqopt$state)
  # expect_names(names(acqopt$state), must.include = "iteration_1")
  # expect_class(acqopt$state$iteration_1, "cma_es.result")
})

test_that("AcqOptimizerCmaes works with 2D", {
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 100L)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
  expect_names(names(acqopt$state), must.include = "iteration_1")
  expect_class(acqopt$state$iteration_1, "cma_es.result")
})

test_that("AcqOptimizerCmaes works with instance", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 100L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrow = 1L)
})

test_that("AcqOptimizerCmaes works with ipop restart", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
   acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "ipop", n_restarts = 3L, population_multiplier = 2L)
  acqfun$surrogate$update()
  acqfun$update()

  res = acqopt$optimize()
  expect_data_table(res, nrows = 1L)
  expect_list(acqopt$state, len = 4L)
  expect_names(names(acqopt$state), identical.to = c("iteration_1", "iteration_2", "iteration_3", "iteration_4"))
  walk(acqopt$state, function(x) expect_class(x, "cma_es.result"))

  ys = map_dbl(acqopt$state, function(x) x$value) * -1
  expect_equal(res$acq_ei, max(ys))
})
