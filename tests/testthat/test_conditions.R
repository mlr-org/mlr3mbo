skip_if_not_installed("mlr3learners")
skip_if_not_installed("DiceKriging")
skip_if_not_installed("rgenoud")

test_that("conditions work", {
  old_threshold_mlr3 = lgr::get_logger("mlr3/core")$threshold
  old_threshold_bbotk = lgr::get_logger("mlr3/bbotk")$threshold
  old_layout = lgr$appenders$console$layout
  lgr::get_logger("mlr3/core")$set_threshold(0)
  lgr::get_logger("mlr3/bbotk")$set_threshold("warn")
  # remove time for snapshots
  lgr$appenders$console$set_layout(
    LayoutFormat$new(fmt = "%L %m %f")
  )

  on.exit({
    lgr$appenders$console$set_layout(old_layout)
    lgr::get_logger("mlr3/core")$set_threshold(old_threshold_mlr3)
    lgr::get_logger("mlr3/bbotk")$set_threshold(old_threshold_bbotk)
  })

  # acq optimizer error
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(OptimizerError$new(), terminator = trm("evals", n_evals = 2L))
  # catches simple error wrapped in Mlr3ErrorMboAcqOptimizer
  expect_snapshot(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer))

  # uncaught acq optimizer error
  acq_optimizer$param_set$set_values(catch_errors = FALSE)
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  expect_error(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer), regexp = "Optimizer Error")

  # surrogate update error
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  learner = LearnerRegrError$new()
  learner$param_set$set_values(error_train = TRUE)
  learner$param_set$set_values(error_predict = FALSE)

  surrogate = SurrogateLearner$new(learner)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = acqo("random_search", terminator = trm("evals", n_evals = 2L))
  expect_snapshot(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer))

  # uncaught surrogate update error
  surrogate$param_set$set_values(catch_errors = FALSE)
  expect_error(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer), class = "Mlr3ErrorLearnerTrain", regexp = "Surrogate Train Error")

  # surrogate predict error
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  learner = LearnerRegrError$new()
  learner$param_set$set_values(error_train = FALSE)
  learner$param_set$set_values(error_predict = TRUE)

  surrogate = SurrogateLearner$new(learner)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = acqo("random_search", terminator = trm("evals", n_evals = 2L))
  expect_snapshot(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer))

  # uncaught surrogate predict error
  acq_optimizer$param_set$set_values(catch_errors = FALSE)
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  expect_error(bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer), class = "Mlr3ErrorLearnerPredict", regexp = "Surrogate Predict Error")
})
