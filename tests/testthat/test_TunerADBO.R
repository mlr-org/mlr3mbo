test_that("adbo tuner works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  tuner = tnr("adbo", design_size = 4)
  tuner$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20L)
  expect_rush_reset(instance$rush)
})

test_that("adbo works with transformation functions", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128, logscale = TRUE),
    cp        = to_tune(1e-04, 1e-1, logscale = TRUE))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", design_size = 4)
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20)
  expect_rush_reset(instance$rush)
})

test_that("search works with dependencies", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(p_int(2, 128, depends = keep_model == TRUE)),
    cp        = to_tune(1e-04, 1e-1),
    keep_model = to_tune())

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", design_size = 4)
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20)
  expect_rush_reset(instance$rush)
})

test_that("adbo works with branching", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("mlr3pipelines")
  flush_redis()
  library(mlr3pipelines)

  graph_learner = as_learner(ppl("branch", graphs = list(rpart = lrn("classif.rpart", id = "rpart"),debug = lrn("classif.debug", id = "debug"))))
  graph_learner$param_set$set_values(
    "rpart.cp" = to_tune(p_dbl(1e-04, 1e-1, depends = branch.selection == "rpart")),
    "rpart.minsplit" = to_tune(p_int(2, 128, depends = branch.selection == "rpart")),
    "debug.x" = to_tune(p_dbl(0, 1, depends = branch.selection == "debug")),
    "branch.selection" = to_tune(c("rpart", "debug"))
  )

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = graph_learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", design_size = 4)
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20)
  expect_rush_reset(instance$rush)
})
