test_that("LFBO Learner can be constructed and trained", {
  l1 = lrn("classif.rpart", predict_type = "prob")
  expect_class(l1, "LearnerClassif")
  l2 = LearnerLFBO$new(l1)
  expect_class(l2, "LearnerRegr")
  expect_true(l2$id == "classif.rpart2regr")
  s1 = SurrogateLearner$new(l2)
  expect_class(s1, "SurrogateLearner")

  t = tsk("boston_housing")
  l2$train(t)
  expect_true(!is.null(l2$state))
  prd = l2$predict(t)
  # Note: Truth value does not make sense!
  expect_class(prd, "PredictionRegr")
})

test_that("LFBO Learner works for optim", {
  set.seed(4321L)
  funs = list(
    fun = function(xs) {
      list(y = xs$x ^ 2)
    },
    fun = function(xs) {
      list(y = - xs$x ^ 2)
    }
  )
  codomains = list(
    ps(y = p_dbl(tags = "minimize")),
    ps(y = p_dbl(tags = "maximize"))
  )
  for (i in seq_len(2)) {
    domain = ps(x = p_dbl(lower = -10, upper = 10))
    fun = funs[[i]]
    codomain = codomains[[i]]
    objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
    instance = OptimInstanceSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 20)
    )
    surrogate = SurrogateLearner$new(LearnerLFBO$new(lrn("classif.ranger", predict_type = "prob", num.trees=2L)))
    acq_function = acqf("mean")
    acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))
    optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer
    )
    optimizer$optimize(instance)
    expect_true(abs(instance$result$y) < .1)
  }
})
