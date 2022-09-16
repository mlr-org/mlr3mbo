test_that("bayesopt_ego class", {
  expect_loop_function(bayesopt_ego)
})

test_that("default bayesopt_ego", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  bayesopt_ego(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})

test_that("bayesopt_ego", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L), acq_function = acq_function)
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
})

test_that("stable bayesopt_ego", {
  # logger stuff
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

  # KM surrogate
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acq_function$surrogate$param_set$values$assert_insample_perf = TRUE
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L), acq_function = acq_function)
  acq_optimizer$param_set$values$logging_level = "info"
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_number(acq_function$surrogate$assert_insample_perf, upper = 1)

  # featureless surrogate with a high perf_threshold of 1
  # this should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  acq_function$surrogate$model = REGR_FEATURELESS
  acq_function$surrogate$param_set$values$perf_threshold = 1
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_error(acq_function$surrogate$assert_insample_perf, regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  lines = readLines(f)
  expect_true(sum(grepl("Current insample performance of the Surrogate Model does not meet the performance threshold", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)

  # KM surrogate but OptimizerError as Optimizer that will fail
  # this again should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  acq_function$surrogate$model = REGR_KM_DETERM
  acq_function$surrogate$param_set$values$perf_threshold = 0
  acq_optimizer = AcqOptimizer$new(OptimizerError$new(), terminator = trm("evals", n_evals = 2L), acq_function = acq_function)
  acq_optimizer$param_set$values$logging_level = "info"
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_number(acq_function$surrogate$assert_insample_perf, upper = 1)
  lines = readLines(f)
  expect_true(sum(grepl("Optimizer Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 2L)

  # Surrogate using LearnerRegrError as Learner that will fail during train
  # this again should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  acq_function$surrogate$model = LearnerRegrError$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L), acq_function = acq_function)
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Surrogate Train Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 3L)

  # Surrogate using LearnerRegrError as Learner that will fail during predict
  # this again should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  acq_function$surrogate$param_set$values$perf_threshold = NULL
  acq_function$surrogate$param_set$values$assert_insample_perf = FALSE
  acq_function$surrogate$model$param_set$values$error_train = FALSE
  acq_optimizer$param_set$values$logging_level = "info"
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Surrogate Predict Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 4L)
})

test_that("bayesopt_ego with trafo", {
  domain = ps(x = p_dbl(lower = 10, upper = 20, trafo = function(x) x - 15))
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2),
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )
  instance = MAKE_INST(objective = objective, search_space = domain, terminator = trm("evals", n_evals = 5L))

  acq_function = AcqFunctionEI$new(surrogate = SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L), acq_function = acq_function)
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
})

test_that("bayesopt_ego eips", {
  objective = ObjectiveRFun$new(
    fun = function(xs) list(y = xs$x ^ 2, time = abs(xs$x)),
    domain = ps(x = p_dbl(lower = -5, upper = 5)),
    codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time")),
    id = "xsq"
  )

  terminator = trm("evals", n_evals = 5L)

  instance = OptimInstanceSingleCrit$new(
    objective = objective,
    terminator = terminator
  )

  surrogate = default_surrogate(instance, n_learner = 2L)
  surrogate$y_cols = c("y", "time")

  acq_function = AcqFunctionEIPS$new()

  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true("acq_eips" %in% names(instance$archive$data))
  expect_equal(names(surrogate$model), c("y", "time"))
})

test_that("bayesopt_ego random interleave", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  acq_function = AcqFunctionEI$new(surrogate = SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L), acq_function = acq_function)
  bayesopt_ego(instance, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = 2L)
  expect_true(nrow(instance$archive$data) == 10L)
  expect_identical(is.na(instance$archive$data$acq_ei), c(rep(TRUE, 4L), FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
})

