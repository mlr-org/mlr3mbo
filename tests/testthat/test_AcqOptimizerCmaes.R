test_that("AcqOptimizerCmaes works", {
  skip_if_missing_regr_km()
  skip_if_not_installed("libcmaesr")
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
  skip_if_missing_regr_km()
  skip_if_not_installed("libcmaesr")
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
})

test_that("AcqOptimizerCmaes works with instance", {
  skip_if_missing_regr_km()
  skip_if_not_installed("libcmaesr")
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 100L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrows = 1L)
})

test_that("AcqOptimizerCmaes is available via the dictionary", {
  skip_if_not_installed("libcmaesr")
  acqopt = acqo("cmaes")
  expect_r6(acqopt, "AcqOptimizerCmaes")
  expect_equal(acqopt$label, "CMA-ES")
  expect_equal(acqopt$man, "mlr3mbo::AcqOptimizerCmaes")
  expect_true("cmaes" %in% mlr_acqoptimizers$keys())
})

test_that("AcqOptimizerCmaes reports the acquisition function value on the original scale", {
  skip_if_missing_regr_km()
  skip_if_not_installed("libcmaesr")
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 100L)
  acqfun$surrogate$update()
  acqfun$update()

  xdt = acqopt$optimize()
  expect_equal(xdt[[acqfun$id]], acqfun$eval_dt(xdt[, acqfun$domain$ids(), with = FALSE])[[acqfun$id]])
  expect_true(xdt[[acqfun$id]] >= 0)
})

test_that("AcqOptimizerCmaes resets state between optimize() calls", {
  skip_if_missing_regr_km()
  skip_if_not_installed("libcmaesr")
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqopt$param_set$set_values(max_fevals = 100L)
  acqfun$surrogate$update()
  acqfun$update()

  acqopt$optimize()
  acqopt$optimize()
  expect_list(acqopt$state)

  acqopt$reset()
  expect_null(acqopt$state)
})

test_that("AcqOptimizerCmaes rejects non-numeric search spaces", {
  skip_if_not_installed("libcmaesr")
  instance = oi(OBJ_1D_MIXED, terminator = trm("evals", n_evals = 5L))
  design = generate_design_random(instance$search_space, n = 5L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_FEATURELESS, archive = instance$archive)
  acqfun = acqf("mean", surrogate = surrogate)
  acqopt = AcqOptimizerCmaes$new(acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()

  expect_error(acqopt$optimize(), "only supports fully numeric")
})

test_that("AcqOptimizerCmaes has skip_already_evaluated enabled by default", {
  acqopt = AcqOptimizerCmaes$new()
  expect_true("skip_already_evaluated" %in% acqopt$param_set$ids())
  expect_true(acqopt$param_set$values$skip_already_evaluated)
})
