test_that("async optimizer works in defaults", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10),
  )
  optimizer = opt("async_mbo", design_function = "sobol", design_size = 5)

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated"))

  expect_rush_reset(instance$rush)
})

test_that("async optimizer works with ei", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10),
  )
  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 6,
    acq_function = acqf("ei"))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_ei", ".already_evaluated"))

  expect_rush_reset(instance$rush)
})

test_that("async optimizer works with ei with epsilon decay", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 26),
    callback = clbk("mlr3mbo.epsilon_decay", rate = 0.1, period = 5L)
  )
  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 6,
    acq_function = acqf("ei", epsilon = 3))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_ei", "epsilon_0", "epsilon", ".already_evaluated"))

  expect_rush_reset(instance$rush)
})

test_that("async optimizer works with cb", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10)
  )
  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acqf("cb", lambda = 3))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated"))

  expect_rush_reset(instance$rush)
})

test_that("async optimizer works with cb with uniformly sampled lambda", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 20),
    callback = clbk("mlr3mbo.sample_lambda_uniform")
  )
  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acqf("cb", lambda = 3))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", "lambda", ".already_evaluated"))
  expect_length(unique(instance$archive$data$lambda), 2)

  expect_rush_reset(instance$rush)
})

test_that("async optimizer works with cb with exponentially sampled lambda", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 20),
    callback = clbk("mlr3mbo.sample_lambda_exponential")
  )
  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acqf("cb", lambda = 3))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", "lambda", ".already_evaluated"))
  expect_length(unique(instance$archive$data$lambda), 2)

  expect_rush_reset(instance$rush)
})

test_that("async mbo works with exponential lambda decay", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10),
    callback = clbk("mlr3mbo.exponential_lambda_decay")
  )
  optimizer = opt("async_mbo",
    design_function = "sobol",
    design_size = 5,
    acq_function = acqf("cb", lambda = 3))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "lambda_0", "lambda"))

  expect_rush_reset(instance$rush)
})
