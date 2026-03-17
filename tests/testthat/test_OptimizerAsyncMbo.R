skip_if_not_installed("rush")
skip_if_no_redis()

test_that("OptimizerAsyncMbo works in defaults", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
    rush = rush
  )
  optimizer = opt("async_mbo", design_function = "sobol", design_size = 5L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb"))
})

test_that("OptimizerAsyncMbo works with evaluations in archive", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 10L),
    rush = rush
  )

  optimizer = opt("async_random_search")
  optimizer$optimize(instance)

  instance$terminator$param_set$values$n_evals = 40L

  optimizer = opt("async_mbo")
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data[is.na(get("acq_cb"))], min.rows = 10L)
  expect_data_table(instance$archive$data[!is.na(get("acq_cb"))], min.rows = 20L)
})
