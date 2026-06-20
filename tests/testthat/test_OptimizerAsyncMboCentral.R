skip_if_not_installed("rush")
skip_if_no_redis()

test_that("OptimizerAsyncMboCentral works in defaults", {
  rush = start_rush(n_workers = 2)
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
  optimizer = opt("async_mbo_central", design_function = "sobol", design_size = 5L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = c("acq_cb"))
})

test_that("OptimizerAsyncMboCentral works with initial design", {
  rush = start_rush(n_workers = 2)
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
  initial_design = generate_design_sobol(PS_2D, n = 5L)$data
  initial_design[, y := OBJ_2D$eval_many(transpose_list(initial_design))]
  initial_design[, extra := "test"]

  optimizer = opt("async_mbo_central", initial_design = initial_design)
  expect_data_table(optimizer$optimize(instance), nrows = 1L)
})

test_that("OptimizerAsyncMboCentral works with evaluations in archive", {
  rush = start_rush(n_workers = 2)
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

  optimizer = opt("async_mbo_central")
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data[is.na(get("acq_cb"))], min.rows = 10L)
  expect_data_table(instance$archive$data[!is.na(get("acq_cb"))], min.rows = 20L)
})

test_that("OptimizerAsyncMboCentral works with custom surrogate", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")

  rush = start_rush(n_workers = 2)
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

  surrogate = default_surrogate(instance)
  acq_function = acqf("cb")
  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))

  optimizer = opt("async_mbo_central",
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    design_function = "sobol",
    design_size = 5L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
})

test_that("OptimizerAsyncMboCentral print method works", {
  optimizer = opt("async_mbo_central")
  expect_output(print(optimizer), "OptimizerAsyncMboCentral")
})

test_that("OptimizerAsyncMboCentral reset method works", {
  optimizer = opt("async_mbo_central")

  # set some values
  optimizer$surrogate = default_surrogate(MAKE_INST())
  optimizer$acq_function = acqf("cb")

  # reset

  optimizer$reset()

  expect_null(optimizer$surrogate)
  expect_null(optimizer$acq_function)
  expect_null(optimizer$acq_optimizer)
  expect_null(optimizer$result_assigner)
})
