test_that("OptimizerADBO works in defaults", {
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
  optimizer = opt("adbo", design_function = "sobol", design_size = 5L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 20L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))

  expect_rush_reset(instance$rush)
})

