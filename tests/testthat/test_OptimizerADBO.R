test_that("search works with decentralized network", {
  flush_redis()
  rush::rush_plan(n_workers = 2)

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", init_design_size = 4)
  optimizer$optimize(instance)

  # instance$rush = RushWorker$new(instance$rush$network_id, host = "local")
  # inst = instance
  # instance$archive$data[order(timestamp_ys)]
  # instance$rush$processes[[1]]$read_all_error_lines()
  # instance$rush$read_log()

  expect_data_table(instance$archive$data, min.rows = 3L)

  expect_rush_reset(instance$rush)
})

test_that("search works with transformation functions", {
  flush_redis()
  library(rush)
  rush_plan(n_workers = 2)

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128, logscale = TRUE),
    cp        = to_tune(1e-04, 1e-1, logscale = TRUE))

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", init_design_size = 4)
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20)

  expect_rush_reset(instance$rush)
})

test_that("search works with dependencies", {
  flush_redis()
  library(rush)
  rush_plan(n_workers = 2)

  learner = lrn("classif.rpart",
    minsplit  = to_tune(p_int(2, 128, depends = keep_model == TRUE)),
    cp        = to_tune(1e-04, 1e-1),
    keep_model = to_tune())

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", init_design_size = 4)
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20)

  expect_rush_reset(instance$rush)
})

test_that("adbo works with branching", {
  flush_redis()
  library(rush)
  library(mlr3pipelines)
  rush_plan(n_workers = 2)

  graph_learner = as_learner(ppl("branch", graphs = list(rpart = lrn("classif.rpart", id = "rpart"),debug = lrn("classif.debug", id = "debug"))))
  graph_learner$param_set$set_values(
    "rpart.cp" = to_tune(p_dbl(1e-04, 1e-1, depends = branch.selection == "rpart")),
    "rpart.minsplit" = to_tune(p_int(2, 128, depends = branch.selection == "rpart")),
    "debug.x" = to_tune(p_dbl(0, 1, depends = branch.selection == "debug")),
    "branch.selection" = to_tune(c("rpart", "debug"))
  )

  instance = TuningInstanceRushSingleCrit$new(
    task = tsk("pima"),
    learner = graph_learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  optimizer = tnr("adbo", init_design_size = 4)
  optimizer$optimize(instance)

  expect_data_table(instance$archive$data, min.rows = 20)

  expect_rush_reset(instance$rush)
})
