skip_if_not_installed("rush")
skip_if_not_installed("mlr3pipelines")
skip_if_no_redis()

test_that("TunerADBOSubspaces tunes a branching graph learner", {
  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  library(mlr3pipelines)

  graph = ppl(
    "branch",
    list(
      rpart = lrn("classif.rpart", cp = to_tune(1e-4, 1, logscale = TRUE)),
      featureless = lrn("classif.featureless", method = to_tune())
    )
  )
  learner = as_learner(graph)
  learner$param_set$set_values(branch.selection = to_tune())

  instance = ti_async(
    task = tsk("penguins"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 12L),
    rush = rush
  )

  subspaces = partition_search_space(
    instance$search_space,
    param = "branch.selection",
    groups = list(rpart = "rpart", featureless = "featureless")
  )

  tuner = tnr(
    "adbo_subspaces",
    subspaces = subspaces,
    n_workers_subspace = c(rpart = 1L, featureless = 1L),
    design_size = 2L
  )
  tuner$optimize(instance)

  data = instance$archive$data
  expect_data_table(data, min.rows = 12L)
  expect_equal(data$.subspace, data$branch.selection)
  expect_set_equal(unique(data$.subspace), c("rpart", "featureless"))
})
