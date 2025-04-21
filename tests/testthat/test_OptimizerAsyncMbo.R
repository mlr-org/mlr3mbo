test_that("OptimizerAsyncMbo works in defaults", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  rush::rush_plan(n_workers = 2L)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )
  optimizer = opt("async_mbo", design_function = "sobol", design_size = 5L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))

  expect_rush_reset(instance$rush)
})

test_that("OptimizerAsyncMbo works with evaluations in archive", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  rush::rush_plan(n_workers = 2L)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
  )

  optimizer = opt("async_random_search")
  optimizer$optimize(instance)


  instance$terminator$param_set$values$n_evals = 40L

  optimizer = opt("async_mbo")
  optimizer$optimize(instance)

  instance$archive
  expect_data_table(instance$archive$data[is.na(get("acq_cb"))], min.rows = 10)
  expect_data_table(instance$archive$data[!is.na(get("acq_cb"))], min.rows = 20)

  expect_rush_reset(instance$rush)
})
