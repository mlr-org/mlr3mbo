test_that("AcqOptimizerRandomSearch works", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerRandomSearch$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 200L)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
  expect_names(names(acqopt$state), must.include = "iteration_1")
  expect_class(acqopt$state$iteration_1$model, "nloptr")
  expect_true(acqopt$state$iteration_1$model$iterations <= 200L)
})

test_that("AcqOptimizerRandomSearch works with 2D", {
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerRandomSearch$new(acq_function = acqfun)
  acqopt$param_set$set_values(n_evals = 200L)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
  expect_names(names(acqopt$state), must.include = "iteration_1")
  expect_class(acqopt$state$iteration_1$model, "nloptr")
  expect_true(acqopt$state$iteration_1$model$iterations <= 200L)
})

test_that("AcqOptimizerRandomSearch works with instance", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerRandomSearch$new(acq_function = acqfun)
  acqopt$param_set$set_values(n_evals = 10L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrow = 1L)
})

