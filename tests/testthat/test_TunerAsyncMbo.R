skip_if_not_installed("rush")
skip_if_no_redis()

test_that("TunerAsyncMbo works", {
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  learner = lrn("classif.rpart", minsplit = to_tune(2L, 128L), cp = to_tune(1e-04, 1e-1))

  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20L),
    store_benchmark_result = FALSE,
    rush = rush
  )

  tuner = tnr("async_mbo", design_size = 4L)

  expect_data_table(tuner$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = "acq_cb")
})

test_that("TunerAsyncMbo works with initial design", {
  learner = set_validate(lrn("classif.debug",
    x = to_tune(0, 1),
    iter = to_tune(1L, 10L, internal = TRUE),
    early_stopping = TRUE), "test")

  # generate initial design
  rush = start_rush(n_workers = 4)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20L),
    store_benchmark_result = FALSE,
    rush = rush
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  initial_design = instance$archive$data[state == "finished", c("x", "classif.ce")]

  # run mbo
  rush$reset()

  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 30L),
    store_benchmark_result = FALSE,
    rush = rush
  )

  tuner = tnr("async_mbo", initial_design = initial_design)

  expect_data_table(tuner$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 21L)
  expect_names(names(instance$archive$data), must.include = "acq_cb")
})
