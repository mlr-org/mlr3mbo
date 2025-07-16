test_that("AcqOptimizerLbfgsb works", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "none")
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$result)
  expect_names(names(acqopt$result), must.include = "iteration_1")
  expect_class(acqopt$result$iteration_1, "nloptr")
})

test_that("AcqOptimizerLbfgsb works with 2D", {
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$result)
  expect_names(names(acqopt$result), must.include = "iteration_1")
  expect_class(acqopt$result$iteration_1, "nloptr")
})

test_that("AcqOptimizerLbfgsb works with instance", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 10L)
  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrow = 1L)
})

test_that("AcqOptimizerLbfgsb works with random restart", {
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "random", n_restarts = 3L, random_restart_size = 20L)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$result, len = 3L)
  expect_names(names(acqopt$result), identical.to = c("iteration_1", "iteration_2", "iteration_3"))
  walk(acqopt$result, function(x) expect_class(x, "nloptr"))
})
