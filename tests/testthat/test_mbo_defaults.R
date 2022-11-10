test_that("default_surrogate", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("ranger")

  # singlecrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST_1D())
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$model, "LearnerRegrKM")
  expect_equal(surrogate$model$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.stability = 1e-08))
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")

  # singlecrit all numeric, noisy
  surrogate = default_surrogate(MAKE_INST_1D_NOISY())
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$model, "LearnerRegrKM")
  expect_equal(surrogate$model$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.estim = TRUE, jitter = 1e-12))
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")

  # twocrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D))
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$model, types = "LearnerRegrKM")
  expect_equal(surrogate$model[[1L]]$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.stability = 1e-08))
  expect_equal(surrogate$model[[1L]]$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values, surrogate$model[[2L]]$param_set$values)
  expect_equal(surrogate$model[[1L]]$encapsulate, surrogate$model[[2L]]$encapsulate)
  expect_equal(surrogate$model[[1L]]$fallback, surrogate$model[[2L]]$fallback)

  # twocrit all numeric, noisy
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2_NOISY, search_space = PS_1D))
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$model, types = "LearnerRegrKM")
  expect_equal(surrogate$model[[1L]]$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.estim = TRUE, jitter = 1e-12))
  expect_equal(surrogate$model[[1L]]$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values, surrogate$model[[2L]]$param_set$values)
  expect_equal(surrogate$model[[1L]]$encapsulate, surrogate$model[[2L]]$encapsulate)
  expect_equal(surrogate$model[[1L]]$fallback, surrogate$model[[2L]]$fallback)

  # singlecrit mixed input
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED))
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$model, "LearnerRegrRanger")
  expect_equal(surrogate$model$param_set$values,
    list(num.threads = 1L, num.trees = 500L, keep.inbag = TRUE, se.method = "jack"))
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")

  # twocrit mixed input
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2_MIXED, search_space = PS_1D_MIXED))
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$model, types = "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values,
    list(num.threads = 1L, num.trees = 500L, keep.inbag = TRUE, se.method = "jack"))
  expect_equal(surrogate$model[[1L]]$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values, surrogate$model[[2L]]$param_set$values)
  expect_equal(surrogate$model[[1L]]$encapsulate, surrogate$model[[2L]]$encapsulate)
  expect_equal(surrogate$model[[1L]]$fallback, surrogate$model[[2L]]$fallback)

  # singlecrit mixed input deps
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED_DEPS))
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$model, "GraphLearner")
  expect_equal(surrogate$model$graph$ids(), c("imputesample", "imputeoor", "regr.ranger"))
  expect_equal(surrogate$model$param_set$values,
    list(imputesample.affect_columns = mlr3pipelines::selector_type("logical"),
         imputeoor.min = TRUE,
         imputeoor.offset = 1,
         imputeoor.multiplier = 2,
         regr.ranger.num.threads = 1L,
         regr.ranger.num.trees = 500L,
         regr.ranger.keep.inbag = TRUE,
         regr.ranger.se.method = "jack"))
  expect_r6(surrogate$model$fallback, "LearnerRegrFeatureless")

  # specify own learner, specify n_objectives, twocrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D), learner = lrn("regr.featureless"), n_learner = 1L)
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$model, "LearnerRegrFeatureless")

  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D), learner = lrn("regr.featureless"), n_learner = 3L)
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$model, types = "LearnerRegrFeatureless")
})

test_that("default_acqfun", {
  instance = MAKE_INST_1D()
  acq = default_acqfun(instance)
  expect_r6(acq, "AcqFunctionEI")
})

test_that("default_acqopt", {
  acqopt = default_acqopt(default_acqfun(MAKE_INST_1D()))
  expect_r6(acqopt, "AcqOptimizer")
  expect_r6(acqopt$optimizer, "OptimizerRandomSearch")
})

test_that("stability and defaults", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
  skip_if_not_installed("ranger")

  console_appender = if (packageVersion("lgr") >= "0.4.0") lg$inherited_appenders$console else lg$inherited_appenders$appenders.console
  f = tempfile("bbotklog_", fileext = "log")
  th1 = lg$threshold
  th2 = console_appender$threshold

  lg$set_threshold("debug")
  lg$add_appender(lgr::AppenderFile$new(f, threshold = "debug"), name = "testappender")
  console_appender$set_threshold("warn")

  on.exit({
    lg$remove_appender("testappender")
    lg$set_threshold(th1)
    console_appender$set_threshold(th2)
  })

  # Surrogate using LearnerRegrError as Learner that will fail during train
  # this should trigger a mbo_error
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  learner = LearnerRegrError$new()
  learner$encapsulate[c("train", "predict")] = "evaluate"
  learner$fallback = lrn("regr.ranger", num.trees = 20L, keep.inbag = TRUE, se.method = "jack")
  surrogate = default_surrogate(instance, learner = learner, n_learner = 1L)
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$model, "LearnerRegrError")
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")
  acq_function = default_acqfun(instance)
  expect_r6(acq_function, "AcqFunctionEI")
  acq_optimizer = default_acqopt(acq_function)
  acq_optimizer$param_set$values$logging_level = "info"
  expect_r6(acq_optimizer, "AcqOptimizer")
  expect_r6(acq_optimizer$optimizer, "OptimizerRandomSearch")

  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_equal(acq_function$surrogate$model$errors, "Surrogate Train Error")
  lines = readLines(f)
  # Nothing should happen here due to the fallback learner
  expect_true(sum(grepl("Surrogate Train Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 0L)

  acq_function$surrogate$model$reset()
  acq_function$surrogate$model$fallback = NULL
  instance$archive$clear()
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  # Training fails but this error is not logged due to the "evaluate" encapsulate
  expect_equal(acq_function$surrogate$model$errors, "Surrogate Train Error")
  expect_true(sum(grepl("Surrogate Train Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 0L)
  expect_true(sum(grepl("Cannot predict", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
})

