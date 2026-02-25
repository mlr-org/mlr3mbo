test_that("default_loop_function", {
  instance = MAKE_INST_1D()
  loop_function = default_loop_function(instance)
  expect_class(loop_function, classes = "loop_function")
  expect_true(attr(loop_function, "id") == "bayesopt_ego")

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D)
  loop_function = default_loop_function(instance)
  expect_class(loop_function, classes = "loop_function")
  expect_true(attr(loop_function, "id") == "bayesopt_smsego")
})

test_that("default_surrogate", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("mlr3pipelines")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
  skip_if_not_installed("ranger")

  # singlecrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST_1D())
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$learner, "LearnerRegrKM")
  expect_equal_sorted(surrogate$learner$param_set$values, list(
    scaling = FALSE,
    covtype = "matern5_2",
    optim.method = "gen",
    control = list(trace = FALSE),
    nugget.stability = 1e-08))
  expect_equal(surrogate$learner$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner$fallback, "LearnerRegrRanger")
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # singlecrit all numeric, noisy
  surrogate = default_surrogate(MAKE_INST_1D_NOISY())
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$learner, "LearnerRegrKM")
  expect_equal_sorted(surrogate$learner$param_set$values, list(
    scaling = FALSE,
    covtype = "matern5_2",
    optim.method = "gen",
    control = list(trace = FALSE),
    nugget.estim = TRUE,
    jitter = 1e-12))
  expect_equal(surrogate$learner$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner$fallback, "LearnerRegrRanger")
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # twocrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D))
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$learner, types = "LearnerRegrKM")
  expect_equal_sorted(surrogate$learner[[1L]]$param_set$values, list(
    scaling = FALSE,
    covtype = "matern5_2",
    optim.method = "gen",
    control = list(trace = FALSE),
    nugget.stability = 1e-08))
  expect_equal(surrogate$learner[[1L]]$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$learner[[1L]]$param_set$values, surrogate$learner[[2L]]$param_set$values)
  expect_equal(surrogate$learner[[1L]]$encapsulation, surrogate$learner[[2L]]$encapsulation)
  expect_equal(surrogate$learner[[1L]]$fallback, surrogate$learner[[2L]]$fallback)
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # twocrit all numeric, noisy
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2_NOISY, search_space = PS_1D))
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$learner, types = "LearnerRegrKM")
  expect_equal_sorted(surrogate$learner[[1L]]$param_set$values, list(
    scaling = FALSE,
    covtype = "matern5_2",
    optim.method = "gen",
    control = list(trace = FALSE),
    nugget.estim = TRUE,
    jitter = 1e-12))
  expect_equal(surrogate$learner[[1L]]$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$learner[[1L]]$param_set$values, surrogate$learner[[2L]]$param_set$values)
  expect_equal(surrogate$learner[[1L]]$encapsulation, surrogate$learner[[2L]]$encapsulation)
  expect_equal(surrogate$learner[[1L]]$fallback, surrogate$learner[[2L]]$fallback)
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # singlecrit mixed input
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED))
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$learner, "LearnerRegrRanger")
  expect_equal_sorted(surrogate$learner$param_set$values, list(
    num.threads = 1L,
    num.trees = 500L,
    se.method = "law_of_total_variance",
    splitrule = "variance",
    keep.inbag = TRUE,
    sample.fraction = 1,
    min.node.size = 3,
    min.bucket = 3,
    mtry.ratio = 5 / 6,
    sigma2.threshold = 1e-2))
  expect_equal(surrogate$learner$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner$fallback, "LearnerRegrRanger")
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # twocrit mixed input
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2_MIXED, search_space = PS_1D_MIXED))
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$learner, types = "LearnerRegrRanger")
  expect_equal_sorted(surrogate$learner[[1]]$param_set$values, list(
    num.threads = 1L,
    num.trees = 500L,
    se.method = "law_of_total_variance",
    splitrule = "variance",
    keep.inbag = TRUE,
    sample.fraction = 1,
    min.node.size = 3,
    min.bucket = 3,
    mtry.ratio = 5 / 6,
    sigma2.threshold = 1e-2))
  expect_equal(surrogate$learner[[1L]]$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$learner[[1L]]$param_set$values, surrogate$learner[[2L]]$param_set$values)
  expect_equal(surrogate$learner[[1L]]$encapsulation, surrogate$learner[[2L]]$encapsulation)
  expect_equal(surrogate$learner[[1L]]$fallback, surrogate$learner[[2L]]$fallback)
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # singlecrit mixed input deps
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED_DEPS))
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$learner, "LearnerRegrRanger")
  expect_equal_sorted(surrogate$learner$param_set$values, list(
    num.threads = 1L,
    num.trees = 500L,
    se.method = "law_of_total_variance",
    splitrule = "variance",
    keep.inbag = TRUE,
    sample.fraction = 1,
    min.node.size = 3,
    min.bucket = 3,
    mtry.ratio = 5 / 6,
    sigma2.threshold = 1e-2))
  expect_equal(surrogate$learner$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner$fallback, "LearnerRegrRanger")
  expect_r6(surrogate$output_trafo, "OutputTrafoLog")

  # specify own learner, specify n_objectives, twocrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D), learner = lrn("regr.featureless"), n_learner = 1L)
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$learner, "LearnerRegrFeatureless")

  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D), learner = lrn("regr.featureless"), n_learner = 3L)
  expect_r6(surrogate, "SurrogateLearnerCollection")
  expect_list(surrogate$learner, types = "LearnerRegrFeatureless")
})

test_that("default_acqfunction", {
  instance = MAKE_INST_1D()
  acq_function = default_acqfunction(instance)
  expect_r6(acq_function, "AcqFunctionCB")

  instance = MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED)
  acq_function = default_acqfunction(instance)
  expect_r6(acq_function, "AcqFunctionCB")

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D)
  acq_function = default_acqfunction(instance)
  expect_r6(acq_function, "AcqFunctionSmsEgo")
})

test_that("default_acqoptimizer", {
  acq_function = default_acqfunction(MAKE_INST_1D())
  acqopt = default_acqoptimizer(acq_function, MAKE_INST_1D())
  expect_r6(acqopt, "AcqOptimizerLocalSearch")
})

test_that("default_result_assigner", {
  results_assigner = default_result_assigner(MAKE_INST_1D())
  expect_r6(results_assigner, "ResultAssignerArchive")
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
  lgr::get_logger("mlr3/core")$set_threshold(0)
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
  learner$predict_type = "se"
  learner$encapsulate("evaluate", lrn("regr.ranger", num.trees = 10L, keep.inbag = TRUE, se.method = "jack", predict_type = "se"))
  surrogate = default_surrogate(instance, learner = learner, n_learner = 1L)
  surrogate$output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
  expect_r6(surrogate, "SurrogateLearner")
  expect_r6(surrogate$learner, "LearnerRegrError")
  expect_equal(surrogate$learner$encapsulation, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$learner$fallback, "LearnerRegrRanger")
  acq_function = default_acqfunction(instance)
  expect_r6(acq_function, "AcqFunctionCB")
  acq_optimizer = acqo(opt("random_search", batch_size = 10L), terminator = trm("evals", n_evals = 10L))
  acq_optimizer$param_set$values$logging_level = "info"
  expect_r6(acq_optimizer, "AcqOptimizer")
  expect_r6(acq_optimizer$optimizer, "OptimizerBatchRandomSearch")

  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_class(acq_function$surrogate$learner$errors[[1]], "Mlr3ErrorLearnerTrain")
  lines = readLines(f)
  # Nothing should happen here due to the fallback learner
  expect_true(sum(grepl("Surrogate Train Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 0L)
})

