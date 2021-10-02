test_that("default bayesopt_soo", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  bayesopt_soo(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})

test_that("bayesopt_soo", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SURR_KM_DETERM)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
})

test_that("stable bayesopt_soo", {
  # logger stuff
  # see mlr-org/mlr3#566
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
  acq_function = AcqFunctionEI$new(surrogate = SURR_KM_DETERM)
  acq_function$surrogate$param_set$values$calc_insample_perf = TRUE
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  acq_optimizer$param_set$values$fix_distance = TRUE
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_number(acq_function$surrogate$assert_insample_perf, upper = 1)

  # featureless surrogate with a high perf_threshold of 1
  # this should trigger a leads_to_exploration_error and log the appropriate error message
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SURR_REGR_FEATURELESS)
  acq_function$surrogate$param_set$values$calc_insample_perf = TRUE
  acq_function$surrogate$param_set$values$perf_threshold = 1
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  acq_optimizer$param_set$values$fix_distance = TRUE
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_error(acq_function$surrogate$assert_insample_perf, regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  lines = readLines(f)
  expect_true(sum(grepl("Current insample performance of the Surrogate Model does not meet the performance threshold", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)

  # KM surrogate but OptimizerError as Optimizer that will fail
  # this again should trigger a leads_to_exploration_error and log the appropriate error message
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SURR_KM_DETERM)
  acq_function$surrogate$param_set$values$calc_insample_perf = TRUE
  acq_optimizer = AcqOptimizer$new(OptimizerError$new(), terminator = trm("evals", n_evals = 2L))
  acq_optimizer$param_set$values$fix_distance = TRUE
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_number(acq_function$surrogate$assert_insample_perf, upper = 1)
  lines = readLines(f)
  expect_true(sum(grepl("Optimizer Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 2L)

  # Surrogate using LearnerRegrError as Learner that will fail during train
  # this again should trigger a leads_to_exploration_error and log the appropriate error message
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SurrogateSingleCritLearner$new(LearnerRegrError$new()))
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Surrogate Train Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 3L)

  # Surrogate using LearnerRegrError as Learner that will fail during predict
  # this again should trigger a leads_to_exploration_error and log the appropriate error message
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SurrogateSingleCritLearner$new(LearnerRegrError$new()))
  acq_function$surrogate$model$param_set$values$error_train = FALSE
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  lines = readLines(f)
  expect_true(sum(grepl("Surrogate Predict Error", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 1L)
  expect_true(sum(grepl("Proposing a randomly sampled point", unlist(map(strsplit(lines, "\\[bbotk\\] "), 2L)))) == 4L)
})

test_that("bayesopt_soo_eips", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 10L))
  acq_function = AcqFunctionEIPS$new(surrogate = SurrogateMultiCritLearners$new(learners = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE))))
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_soo(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
})

