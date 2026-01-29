test_that("TunerAsyncMbo works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2L, 128L),
    cp        = to_tune(1e-04, 1e-1))

  mirai::daemons(4L)
  rush::rush_plan(n_workers = 4L, worker_type = "remote")
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20L),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_mbo", design_size = 4L)

  expect_data_table(tuner$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(names(instance$archive$data), must.include = "acq_cb")

  expect_rush_reset(instance$rush)
})


test_that("TunerAsyncMbo works with initial design", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not(redis_available())
  flush_redis()

  learner = set_validate(lrn("classif.debug",
    x = to_tune(0, 1),
    iter = to_tune(1L, 10L, internal = TRUE),
    early_stopping = TRUE), "test")

  mirai::daemons(4L)

  # generate initial design
  rush::rush_plan(n_workers = 4L, worker_type = "remote")
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20L),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  initial_design = instance$archive$data[state == "finished", c("x", "classif.ce")]

  # run mbo
  flush_redis()

  rush::rush_plan(n_workers = 4L, worker_type = "remote")
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 30L),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_mbo", initial_design = initial_design)

  expect_data_table(tuner$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 21L)
  expect_names(names(instance$archive$data), must.include = "acq_cb")

  expect_rush_reset(instance$rush)
})
