test_that("SurrogateLearner API works", {
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)
  surrogate = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  expect_r6(surrogate$archive, "Archive")
  expect_equal(surrogate$cols_x, "x")
  expect_equal(surrogate$cols_y, "y")
  surrogate$update()
  expect_learner(surrogate$learner)

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_list(pred, len = 2L)
  expect_names(names(pred), identical.to = c("mean", "se"))
  expect_numeric(pred$mean, len = 5L)
  expect_numeric(pred$se, len = 5L)

  # upgrading error class works
  surrogate = SurrogateLearner$new(LearnerRegrError$new(), archive = inst$archive)
  expect_error(surrogate$update(), class = "Mlr3ErrorMboSurrogateUpdate")

  surrogate$param_set$values$catch_errors = FALSE
  expect_error(surrogate$optimize(), class = "simpleError")

  # predict_type
  expect_equal(surrogate$predict_type, surrogate$learner$predict_type)
  surrogate$learner$predict_type = "response"
  expect_equal(surrogate$predict_type, surrogate$learner$predict_type)
  expect_error({surrogate$predict_type = "response"}, "is read-only")
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
  surrogate = SurrogateLearner$new(learner = learner, archive = inst$archive)
  surrogate$update()
  expect_named(surrogate$predict(xdt), "mean")
})

test_that("param_set", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("catch_errors", "impute_method"))
  expect_equal(surrogate$param_set$class[["catch_errors"]], "ParamLgl")
  expect_equal(surrogate$param_set$class[["impute_method"]], "ParamFct")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")
})

test_that("deep clone", {
  inst = MAKE_INST_1D()
  surrogate1 = SurrogateLearner$new(learner = REGR_FEATURELESS, archive = inst$archive)
  surrogate2 = surrogate1$clone(deep = TRUE)
  expect_true(address(surrogate1) != address(surrogate2))
  expect_true(address(surrogate1$learner) != address(surrogate2$learner))
  expect_true(address(surrogate1$archive) != address(surrogate2$archive))
  inst$eval_batch(MAKE_DESIGN(inst))
  expect_true(address(surrogate1$archive$data) != address(surrogate2$archive$data))
})

test_that("packages", {
  skip_if_missing_regr_km()
  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM)
  expect_equal(surrogate$packages, surrogate$learner$packages)
})

test_that("feature types", {
  skip_if_missing_regr_km()
  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM)
  expect_equal(surrogate$feature_types, surrogate$learner$feature_types)
})

