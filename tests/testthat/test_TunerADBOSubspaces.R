skip_if_not_installed("rush")
skip_if_not_installed("mlr3pipelines")
skip_if_no_redis()

test_that("TunerADBOSubspaces tunes a branching graph learner", {
  profiles = c(rpart = 1, featureless = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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

  tuner = tnr("adbo_subspaces", subspaces = subspaces, design_size = 2L)
  tuner$optimize(instance)

  data = instance$archive$data
  expect_data_table(data, min.rows = 12L)
  # `.subspace` is written when an evaluation finishes, so pending evaluations still have `NA`
  finished = data[data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch.selection)
  expect_set_equal(unique(finished$.subspace), c("rpart", "featureless"))
  expect_r6(tuner$surrogate, "SurrogateLearner")
})

test_that("TunerADBOSubspaces fixes the surrogate, acquisition function, and acquisition function optimizer", {
  tuner = tnr("adbo_subspaces")

  expect_error(
    {
      tuner$surrogate = SurrogateLearner$new(REGR_FEATURELESS)
    },
    "read-only"
  )
  expect_error(
    {
      tuner$acq_function = acqf("ei")
    },
    "read-only"
  )
  expect_error(
    {
      tuner$acq_optimizer = acqo(opt("random_search"), trm("evals", n_evals = 100L))
    },
    "read-only"
  )
})
