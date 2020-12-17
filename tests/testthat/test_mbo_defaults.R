test_that("default_surrogate", {
  # singlecrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST_1D())
  expect_r6(surrogate, "SurrogateSingleCrit")
  expect_r6(surrogate$model, "LearnerRegrKM")
  expect_equal(surrogate$model$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", nugget.stability = 1e-08))
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")

  # singlecrit all numeric, noisy
  surrogate = default_surrogate(MAKE_INST_1D_NOISY())
  expect_r6(surrogate, "SurrogateSingleCrit")
  expect_r6(surrogate$model, "LearnerRegrKM")
  expect_equal(surrogate$model$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", nugget.estim = TRUE, jitter = 1e-12))
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")

  # twocrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D))
  expect_r6(surrogate, "SurrogateMultiCrit")
  expect_list(surrogate$model, types = "LearnerRegrKM")
  expect_equal(surrogate$model[[1L]]$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", nugget.stability = 1e-08))
  expect_equal(surrogate$model[[1L]]$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values, surrogate$model[[2L]]$param_set$values)
  expect_equal(surrogate$model[[1L]]$encapsulate, surrogate$model[[2L]]$encapsulate)
  #expect_equal(surrogate$model[[1L]]$fallback, surrogate$model[[2L]]$fallback)

  # twocrit all numeric, noisy
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2_NOISY, search_space = PS_1D))
  expect_r6(surrogate, "SurrogateMultiCrit")
  expect_list(surrogate$model, types = "LearnerRegrKM")
  expect_equal(surrogate$model[[1L]]$param_set$values,
    list(covtype = "matern3_2", optim.method = "gen", nugget.estim = TRUE, jitter = 1e-12))
  expect_equal(surrogate$model[[1L]]$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values, surrogate$model[[2L]]$param_set$values)
  expect_equal(surrogate$model[[1L]]$encapsulate, surrogate$model[[2L]]$encapsulate)
  #expect_equal(surrogate$model[[1L]]$fallback, surrogate$model[[2L]]$fallback)

  # singlecrit mixed input
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED))
  expect_r6(surrogate, "SurrogateSingleCrit")
  expect_r6(surrogate$model, "LearnerRegrRanger")
  expect_equal(surrogate$model$param_set$values,
    list(num.trees = 500L, keep.inbag = TRUE))
  expect_equal(surrogate$model$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model$fallback, "LearnerRegrRanger")

  # twocrit mixed input
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2_MIXED, search_space = PS_1D_MIXED))
  expect_r6(surrogate, "SurrogateMultiCrit")
  expect_list(surrogate$model, types = "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values,
    list(num.trees = 500L, keep.inbag = TRUE))
  expect_equal(surrogate$model[[1L]]$encapsulate, c(train = "evaluate", predict = "evaluate"))
  expect_r6(surrogate$model[[1L]]$fallback, "LearnerRegrRanger")
  expect_equal(surrogate$model[[1L]]$param_set$values, surrogate$model[[2L]]$param_set$values)
  expect_equal(surrogate$model[[1L]]$encapsulate, surrogate$model[[2L]]$encapsulate)
  #expect_equal(surrogate$model[[1L]]$fallback, surrogate$model[[2L]]$fallback)

  # singlecrit mixed input deps
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_MIXED, search_space = PS_1D_MIXED_DEPS))
  expect_r6(surrogate, "SurrogateSingleCrit")
  expect_r6(surrogate$model, "GraphLearner")
  expect_equal(surrogate$model$graph$ids(), c("imputeoor", "regr.ranger"))
  expect_equal(surrogate$model$param_set$values,
    list(imputeoor.min = TRUE, imputeoor.offset = 1, imputeoor.multiplier = 1, regr.ranger.num.trees = 500L, regr.ranger.keep.inbag = TRUE))
  expect_r6(surrogate$model$fallback, "LearnerRegrFeatureless")

  # specify own learner, specify n_objectives, twocrit all numeric, deterministic
  surrogate = default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D), learner = lrn("regr.featureless"), n_objectives = 1L)
  expect_r6(surrogate, "SurrogateSingleCrit")
  expect_r6(surrogate$model, "LearnerRegrFeatureless")

  expect_error(default_surrogate(MAKE_INST(OBJ_1D_2, search_space = PS_1D), learner = lrn("regr.featureless"), n_objectives = 3L),
    regexp = "Assertion on 'n_objectives' failed: Element 1 is not <= 2.")
})



test_that("default_acqfun", {
  expect_r6(default_acqfun(MAKE_INST_1D()), "AcqFunctionEI")
  acq = default_acqfun(MAKE_INST_1D(), surrogate = SurrogateSingleCritLearner$new(lrn("regr.featureless")))
  expect_r6(acq, "AcqFunctionEI")
  expect_r6(acq$surrogate$model, "LearnerRegrFeatureless")
})



test_that("default_acq_optimizer", {
  acqopt = default_acq_optimizer(MAKE_INST_1D())
  expect_r6(acqopt, "AcqOptimizer")
  expect_r6(acqopt$optimizer, "OptimizerRandomSearch")
})

