skip_if_not_installed("rush")
skip_if_no_redis()

test_that("TunerADBO works", {
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

  tuner = tnr("adbo", design_size = 10L)

  expect_data_table(tuner$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, min.rows = 10L)
  expect_names(
    names(instance$archive$data),
    must.include = c("acq_cb", ".already_evaluated", "acq_lambda_0", "acq_lambda")
  )
})
