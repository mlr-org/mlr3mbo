test_that("AcqFunctionStochasticCB works in defaults", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  options(bbotk.debug = TRUE)

  mirai::daemons(1L)
  rush::rush_plan(n_workers = 1L, worker_type = "remote")
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  acq_function = acqf("stochastic_cb")

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))

  expect_rush_reset(instance$rush)
})

test_that("AcqFunctionStochasticCB works with uniform sampling", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  mirai::daemons(2L)
  rush::rush_plan(n_workers = 2L, worker_type = "remote")
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  acq_function = acqf("stochastic_cb", distribution = "uniform", min_lambda = 1, max_lambda = 3)

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))
  expect_numeric(instance$archive$data$acq_lambda, lower = 1, upper = 3)

  expect_rush_reset(instance$rush)
})

test_that("AcqFunctionStochasticCB works with exponential sampling", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  mirai::daemons(2L)
  rush::rush_plan(n_workers = 2L, worker_type = "remote")
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 50L),
  )

  acq_function = acqf("stochastic_cb", distribution = "exponential", lambda = 1.96)

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))
  expect_numeric(unique(instance$archive$data$acq_lambda), len = 3L) # NA + 2

  expect_rush_reset(instance$rush)
})


test_that("AcqFunctionStochasticCB works with lambda decay", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  mirai::daemons(1L)
  rush::rush_plan(n_workers = 1L, worker_type = "remote")
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  acq_function = acqf("stochastic_cb", rate = 0.5)

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))

  expect_numeric(-instance$archive$data$acq_lambda, sorted = TRUE)

  expect_rush_reset(instance$rush)
})

test_that("AcqFunctionStochasticCB works with periodic lambda decay", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  mirai::daemons(1L)
  rush::rush_plan(n_workers = 1L, worker_type = "remote")
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  acq_function = acqf("stochastic_cb", rate = 0.5, period = 2)

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))

  #expect_numeric(unique(instance$archive$data$acq_lambda), len = 3L)
  expect_rush_reset(instance$rush)
})
