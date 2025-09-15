test_that("AcqOptimizerCmaes works", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 100L)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
})

test_that("AcqOptimizerCmaes works with 2D", {
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 100L)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
  expect_list(acqopt$state)
})

test_that("AcqOptimizerCmaes works with instance", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 100L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrow = 1L)
})

