test_that("adbo optimizer works in defaults", {
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

test_that("adbo optimizer works with ei", {
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
    design_size = 5,
    acq_function = acqf("ei"))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_ei", ".already_evaluated"))

  expect_rush_reset(instance$rush)
})

test_that("adbo optimizer works with cb with fixed lambda", {
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
    design_size = 5,
    acq_function = acqf("cb", lambda = 3))

  expect_data_table(optimizer$optimize(instance), nrows = 1)
  expect_data_table(instance$archive$data, min.rows = 10)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated"))

  expect_rush_reset(instance$rush)
})

