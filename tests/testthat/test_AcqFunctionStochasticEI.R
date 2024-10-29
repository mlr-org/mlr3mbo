test_that("AcqFunctionStochasticEI works in defaults", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 1)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10),
  )

  acq_function = acqf("stochastic_ei")

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acq_function)

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c(".already_evaluated", "acq_ei", "epsilon_0", "epsilon"))
  expect_numeric(-instance$archive$data$epsilon, sorted = TRUE)

  expect_rush_reset(instance$rush)
})

test_that("AcqFunctionStochasticEI works with multiple workers", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 20),
  )

  acq_function = acqf("stochastic_ei")

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acq_function)

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c(".already_evaluated", "acq_ei", "epsilon_0", "epsilon"))

  expect_rush_reset(instance$rush)
})


test_that("AcqFunctionStochasticEI works with periodic epsilon decay", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 1)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10),
  )

  acq_function = acqf("stochastic_ei", rate = 0.5, period = 2)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acq_function)

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c(".already_evaluated", "acq_ei", "epsilon_0", "epsilon"))
  expect_numeric(unique(instance$archive$data$epsilon), len = 3)

  expect_rush_reset(instance$rush)
})
