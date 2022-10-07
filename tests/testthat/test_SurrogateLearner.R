test_that("SurrogateLearner API works", {
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)
  surrogate = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  expect_r6(surrogate$archive, "Archive")
  expect_equal(surrogate$x_cols, "x")
  expect_equal(surrogate$y_cols, "y")
  surrogate$update()
  expect_learner(surrogate$model)

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_data_table(pred, col.names = "named", nrows = 5, ncols = 2, any.missing = FALSE)
  expect_named(pred, c("mean", "se"))

  # upgrading error class works
  surrogate = SurrogateLearner$new(LearnerRegrError$new(), archive = inst$archive)
  expect_error(surrogate$update(), class = "surrogate_update_error")
  
  surrogate$param_set$values$catch_errors = FALSE
  expect_error(surrogate$optimize(), class = "simpleError")
})

test_that("predict_types are recognized", {
  skip_if_not_installed("rpart")
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  surrogate = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_named(surrogate$predict(xdt), c("mean", "se"))

  learner = lrn("regr.rpart")
  learner$predict_type = "response"
  surrogate = SurrogateLearner$new(learner = lrn("regr.rpart"), archive = inst$archive)
  surrogate$update()
  expect_named(surrogate$predict(xdt), "mean")
})

test_that("param_set", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("assert_insample_perf", "perf_measure", "perf_threshold", "catch_errors"))
  expect_r6(surrogate$param_set$params$assert_insample_perf, "ParamLgl")
  expect_r6(surrogate$param_set$params$perf_measure, "ParamUty")
  expect_r6(surrogate$param_set$params$perf_threshold, "ParamDbl")
  expect_r6(surrogate$param_set$params$catch_errors, "ParamLgl")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")
})

test_that("insample_perf", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM, archive = inst$archive)
  expect_error({surrogate$insample_perf = 0}, regexp = "insample_perf is read-only.")
  expect_error({surrogate$assert_insample_perf = 0}, regexp = "assert_insample_perf is read-only.")

  surrogate$update()
  expect_equal(surrogate$insample_perf, NaN)

  surrogate$param_set$values$assert_insample_perf = TRUE
  surrogate$param_set$values$perf_threshold = 0
  surrogate$param_set$values$perf_measure = mlr_measures$get("regr.rsq")
  surrogate$update()
  expect_number(surrogate$insample_perf, lower = -Inf, upper = 1)
  expect_equal(names(surrogate$insample_perf), surrogate$param_set$values$perf_measure$id)

  surrogate_constant = SurrogateLearner$new(learner = lrn("regr.featureless"), archive = inst$archive)
  surrogate_constant$param_set$values$assert_insample_perf = TRUE
  surrogate_constant$param_set$values$perf_threshold = 0.5
  surrogate$param_set$values$perf_measure = mlr_measures$get("regr.rsq")
  expect_error(surrogate_constant$update(), regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  expect_number(surrogate_constant$insample_perf, lower = -Inf, upper = 1)
  expect_true(surrogate_constant$insample_perf <= 1e-3)
  expect_equal(names(surrogate_constant$insample_perf), surrogate$param_set$values$perf_measure$id)
})

test_that("deep clone", {
  inst = MAKE_INST_1D()
  surrogate1 = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  surrogate2 = surrogate1$clone(deep = TRUE)
  expect_true(address(surrogate1) != address(surrogate2))
  expect_true(address(surrogate1$model) != address(surrogate2$model))
  expect_true(address(surrogate1$archive) != address(surrogate2$archive))
  inst$eval_batch(MAKE_DESIGN(inst))
  expect_true(address(surrogate1$archive$data) != address(surrogate2$archive$data))
})

test_that("packages", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM)
  expect_equal(surrogate$packages, surrogate$model$packages)
})

test_that("feature types", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM)
  expect_equal(surrogate$feature_types, surrogate$model$feature_types)
})

