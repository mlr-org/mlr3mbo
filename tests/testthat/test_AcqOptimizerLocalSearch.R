test_that("AcqOptimizerLocalSearch works", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLocalSearch$new(acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()

  res = acqopt$optimize()
  expect_data_table(res, nrows = 1L, ncols = 2L)
  expect_names(names(res), must.include = c("x", "acq_ei"))
})

test_that("AcqOptimizerLocalSearch works with 2D", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLocalSearch$new(acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()

  res = acqopt$optimize()
  expect_data_table(res, nrows = 1L, ncols = 3L)
  expect_names(names(res), must.include = c("x1", "x2", "acq_ei"))
})

test_that("AcqOptimizerLocalSearch works with instance", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLocalSearch$new(acq_function = acqfun)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrows = 1L)
})

