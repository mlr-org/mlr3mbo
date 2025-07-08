test_that("AcqOptimizerDirect works", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerDirect$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)
})

test_that("AcqOptimizerDirect works with 2D", {
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerDirect$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)
})

test_that("AcqOptimizerDirect works", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerDirect$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 100L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  optimizer$optimize(instance)

  instance$archive$data
})
