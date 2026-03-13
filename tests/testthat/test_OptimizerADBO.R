skip_if_not_installed("rush")
skip_if_no_redis()

test_that("OptimizerADBO works in defaults", {
  rush = start_rush(n_workers = 2L)
  on.exit({rush$reset(); mirai::daemons(0)})

  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 20L),
    rush = rush,
  )
  optimizer = opt("adbo", design_function = "sobol", design_size = 5L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 20L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda"))
})
