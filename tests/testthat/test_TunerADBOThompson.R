skip_if_not_installed("rush")
skip_if_not_installed("mlr3pipelines")
skip_if_no_redis()

test_that("TunerADBOThompson tunes a branching graph learner on a shared compute profile", {
  profiles = c(cpu = 2)
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
    terminator = trm("evals", n_evals = 16L),
    rush = rush
  )

  subspaces = partition_search_space(instance$search_space, param = "branch.selection")

  tuner = tnr(
    "adbo_thompson",
    subspaces = subspaces,
    subspace_profiles = c(rpart = "cpu", featureless = "cpu"),
    design_size = 2L
  )
  surrogate = default_surrogate(instance)
  surrogate$param_set$set_values(catch_errors = FALSE)
  tuner$surrogate = surrogate
  tuner$optimize(instance)

  data = instance$archive$data
  expect_data_table(data, min.rows = 16L)
  finished = data[data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch.selection)
  expect_set_equal(unique(finished$.subspace), c("rpart", "featureless"))
  expect_identical(tuner$surrogate, surrogate)
})

test_that("TunerADBOThompson is registered", {
  tuner = tnr("adbo_thompson")
  expect_r6(tuner, "TunerADBOThompson")
  expect_equal(tuner$param_set$values$alpha, 1)
  expect_man_exists(tuner$man)
})
