test_that("AcqFunctionStochasticEI works in defaults", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  rush::rush_plan(n_workers = 1L)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  acq_function = acqf("stochastic_ei")

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c(".already_evaluated", "acq_ei", "acq_epsilon_0", "acq_epsilon"))
  expect_numeric(-instance$archive$data$acq_epsilon, sorted = TRUE)

  expect_rush_reset(instance$rush)
})

test_that("AcqFunctionStochasticEI works with multiple workers", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  rush::rush_plan(n_workers = 2L)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 20L),
  )

  acq_function = acqf("stochastic_ei")

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c(".already_evaluated", "acq_ei", "acq_epsilon_0", "acq_epsilon"))

  expect_rush_reset(instance$rush)
})


test_that("AcqFunctionStochasticEI works with periodic epsilon decay", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  rush::rush_plan(n_workers = 1L)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  acq_function = acqf("stochastic_ei", rate = 0.5, period = 2)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c(".already_evaluated", "acq_ei", "acq_epsilon_0", "acq_epsilon"))
  expect_numeric(unique(instance$archive$data$acq_epsilon), len = 3L)

  expect_rush_reset(instance$rush)
})
