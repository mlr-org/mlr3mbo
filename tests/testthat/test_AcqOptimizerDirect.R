test_that("AcqOptimizerDirect works", {
  skip_if_missing_regr_km()
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
  expect_class(acqopt$state, "nloptr")
  expect_true(acqopt$state$iterations <= 200L)
})

test_that("AcqOptimizerDirect works with 2D", {
  skip_if_missing_regr_km()
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
  expect_class(acqopt$state, "nloptr")
  expect_true(acqopt$state$iterations <= 200L)
})

test_that("AcqOptimizerDirect works with instance", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerDirect$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 10L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrows = 1L)
})

test_that("AcqOptimizerDirect does not expose the dead minf_max parameter", {
  expect_false("minf_max" %in% AcqOptimizerDirect$new()$param_set$ids())
})

test_that("AcqOptimizerDirect maxeval accepts -1L and -1 to deactivate", {
  acqopt = AcqOptimizerDirect$new()
  expect_error(acqopt$param_set$set_values(maxeval = -1L), NA)
  expect_error(acqopt$param_set$set_values(maxeval = -1), NA)
})

test_that("AcqOptimizerDirect resets state between optimize() calls", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerDirect$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L)
  acqfun$surrogate$update()
  acqfun$update()

  acqopt$optimize()
  acqopt$optimize()
  expect_class(acqopt$state, "nloptr")

  acqopt$reset()
  expect_null(acqopt$state)
})

test_that("AcqOptimizerDirect has no restart parameters", {
  acqopt = AcqOptimizerDirect$new()
  expect_true("restart_strategy" %nin% acqopt$param_set$ids())
  expect_true("max_restarts" %nin% acqopt$param_set$ids())
})
