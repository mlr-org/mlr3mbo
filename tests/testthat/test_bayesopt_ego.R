test_that("bayesopt_ego class", {
  expect_loop_function(bayesopt_ego)
})

test_that("bayesopt_ego", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(sum(is.na(instance$archive$data$acq_ei)) == 4L)
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
})

test_that("bayesopt_ego custom initial design and sizes", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 6L))
  design = generate_design_random(instance$search_space, n = 5L)$data
  instance$eval_batch(design)
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 6L)
  expect_true(sum(is.na(instance$archive$data$acq_ei)) == 5L)
  expect_true(!is.na(instance$archive$data$acq_ei[6L]))

  instance$archive$clear()
  instance$eval_batch(design)
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, init_design_size = 10L)  # ignored
  expect_true(nrow(instance$archive$data) == 6L)
  expect_true(sum(is.na(instance$archive$data$acq_ei)) == 5L)
  expect_true(!is.na(instance$archive$data$acq_ei[6L]))

  instance$archive$clear()
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, init_design_size = 3L)
  expect_true(nrow(instance$archive$data) == 6L)
  expect_true(sum(is.na(instance$archive$data$acq_ei)) == 3L)
  expect_true(!is.na(instance$archive$data$acq_ei[4L]))
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
  expect_true(!is.na(instance$archive$data$acq_ei[6L]))

  instance$archive$clear()
  expect_error(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, init_design_size = 6L), "terminated")
})

test_that("stable bayesopt_ego", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

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
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  acq_optimizer$param_set$values$logging_level = "info"
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)

  # KM surrogate but OptimizerError as Optimizer that will fail
  # this should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_optimizer = AcqOptimizer$new(OptimizerError$new(), terminator = trm("evals", n_evals = 2L))
  acq_optimizer$param_set$values$logging_level = "info"
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Optimizer Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)

  # Surrogate using LearnerRegrError as Learner that will fail during train
  # this should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  surrogate = SurrogateLearner$new(LearnerRegrError$new())
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Surrogate Train Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 2L)

  # Surrogate using LearnerRegrError as Learner that will fail during predict
  # this should trigger a mbo_error and log the appropriate error message
  instance$archive$clear()
  surrogate = SurrogateLearner$new(LearnerRegrError$new())
  surrogate$learner$param_set$values$error_train = FALSE
  acq_optimizer$param_set$values$logging_level = "info"
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Surrogate Predict Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 3L)
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

  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
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

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = terminator
  )

  surrogate = default_surrogate(instance, learner = REGR_FEATURELESS, n_learner = 2L)
  surrogate$cols_y = c("y", "time")

  acq_function = AcqFunctionEIPS$new()

  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true("acq_eips" %in% names(instance$archive$data))
  expect_equal(names(surrogate$learner), c("y", "time"))
})

test_that("bayesopt_ego random interleave", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = 2L)
  expect_true(nrow(instance$archive$data) == 10L)
  expect_identical(is.na(instance$archive$data$acq_ei), c(rep(TRUE, 4L), FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
})

