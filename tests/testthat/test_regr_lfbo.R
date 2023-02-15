test_that("LearnerRegrLFBO can be constructed and trained and setting hyperparameters works", {
  learner_classif = lrn("classif.rpart", predict_type = "prob")
  expect_class(learner_classif, "LearnerClassif")
  learner = LearnerRegrLFBO$new(learner_classif)
  expect_class(learner, "LearnerRegr")
  expect_learner(learner)
  expect_true(learner$id == "regr.lfbo")
  surrogate = SurrogateLearner$new(learner)
  expect_class(surrogate, "SurrogateLearner")

  task = tsk("mtcars")
  expect_error(learner$train(task), regexp = "surrogate_max_to_min must be set")
  expect_error({learner$surrogate_max_to_min = 10}, regexp = "Must be a subset")
  learner$surrogate_max_to_min = 1
  learner$train(task)
  expect_true(!is.null(learner$state))
  prediction = learner$predict(task)
  expect_class(prediction, "PredictionRegr")  # Note: Truth value does not make sense!
  expect_true(cor(prediction$truth, prediction$response) < - 0.5)

  learner$surrogate_max_to_min = -1
  learner$train(task)
  expect_true(!is.null(learner$state))
  prediction = learner$predict(task)
  expect_class(prediction, "PredictionRegr")  # Note: Truth value does not make sense!
  expect_true(cor(prediction$truth, prediction$response) > 0.5)

  lfbo_param_set = ps(wt = p_fct(levels = c("ei", "pi"), default = "ei", tags = "train"), gamma = p_dbl(lower = 0, upper = 1, default = 1/3, tags = "train"))
  lfbo_param_set$set_id = "lfbo"
  param_set = ParamSetCollection$new(list(lfbo_param_set, learner_classif$param_set))
  expect_equal(param_set, learner$param_set)


  learner$param_set$values$lfbo.wt = "pi"
  learner$param_set$values$lfbo.gamma = 0.5
  learner$param_set$values$maxdepth = 20L

  expect_true(learner_classif$param_set$values$maxdepth == 20L)

  learner$train(task)
  expect_true(!is.null(learner$state))
  prediction = learner$predict(task)
  expect_class(prediction, "PredictionRegr")  # Note: Truth value does not make sense!
})

test_that("Likelihood Free Bayesian Optimization", {
  with_seed(2906, {
    instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
    surrogate = SurrogateLearner$new(lrn("regr.lfbo", lrn("classif.ranger", predict_type = "prob", num.trees = 10L)))
    acq_function = acqf("lfbo")
    acq_optimizer = acqo(optimizer = opt("random_search", batch_size = 100L), terminator = trm("evals", n_evals = 100L))
    optimizer = opt("mbo",
      loop_function = bayesopt_ego,
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = acq_optimizer
    )
    optimizer$optimize(instance)
    expect_true(abs(instance$result$y) < 0.1)

    # maximization of - y
    fun = function(xs) {
      list(y = - as.numeric(xs)^2)
    }
    codomain = ParamSet$new(list(ParamDbl$new("y", tags = "maximize")))
    objective = ObjectiveRFun$new(fun = fun, domain = PS_1D, codomain = codomain, properties = "single-crit")
    instance = MAKE_INST(objective = objective, search_space = PS_1D, terminator = trm("evals", n_evals = 10L))
    optimizer$optimize(instance)
    expect_true(abs(instance$result$y) < 0.1)
  })
})
