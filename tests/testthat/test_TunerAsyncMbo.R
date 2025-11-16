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

