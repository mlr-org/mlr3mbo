skip_if_not_installed("rush")
skip_if_no_redis()

test_that("AcqFunctionStochasticEI works in defaults", {
  rush = start_rush(n_workers = 1)
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

  acq_function = acqf("stochastic_ei")

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt(
    "async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(
    names(instance$archive$data),
    must.include = c("acq_ei", ".already_evaluated", "acq_epsilon_0", "acq_epsilon")
  )
  expect_numeric(-instance$archive$data$acq_epsilon, sorted = TRUE)
})

test_that("AcqFunctionStochasticEI works with multiple workers", {
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 20L),
    rush = rush
  )

  acq_function = acqf("stochastic_ei")

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt(
    "async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
<<<<<<< HEAD
  expect_names(
    names(instance$archive$data),
    must.include = c("acq_ei", ".already_evaluated", "acq_epsilon_0", "acq_epsilon")
  )
=======
  expect_names(names(instance$archive$data), must.include = c("acq_ei", ".already_evaluated", "acq_epsilon_0", "acq_epsilon"))
>>>>>>> main
})


test_that("AcqFunctionStochasticEI works with periodic epsilon decay", {
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

  acq_function = acqf("stochastic_ei", rate = 0.5, period = 2)

  acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 10L), acq_function = acq_function)

  optimizer = opt(
    "async_mbo",
    design_function = "sobol",
    design_size = 5L,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(
    names(instance$archive$data),
    must.include = c("acq_ei", ".already_evaluated", "acq_epsilon_0", "acq_epsilon")
  )
  expect_numeric(unique(instance$archive$data$acq_epsilon), len = 3L)
})
