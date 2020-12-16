test_that("SurrogateSingleCritLearner API works", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = "y")
  expect_learner(surrogate$model)

  xdt = data.table(x = seq(5))
  pred = surrogate$predict(xdt)
  expect_data_table(pred, col.names = "named", nrows = 5, ncols = 2,
    any.missing = FALSE)
  expect_named(pred, c("mean", "se"))

  expect_error(surrogate$update(xydt = xydt, y_cols = c("y1", "y2")),
    fixed = "Assertion on 'target' failed: Must have length 1")
})

test_that("predict_types are recognized", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = "y")

  xdt = data.table(x = seq(5))
  expect_named(surrogate$predict(xdt), c("mean", "se"))

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.rpart"))
  surrogate$update(xydt = xydt, y_cols = "y")

  surrogate$predict(xdt)
  expect_named(surrogate$predict(xdt), c("mean"))
})

test_that("param_set", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("performance_measure", "performance_epsilon"))
  expect_r6(surrogate$param_set$params$performance_measure, "ParamUty")
  expect_r6(surrogate$param_set$params$performance_epsilon, "ParamDbl")
})

test_that("insample_performance", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = "y")
  expect_double(surrogate$insample_performance, lower =  -Inf, upper = 1, any.missing = FALSE, len = 1L)
  expect_equal(names(surrogate$insample_performance), surrogate$param_set$values$performance_measure$id)

  surrogate_constant = SurrogateSingleCritLearner$new(learner = lrn("regr.featureless"))
  surrogate_constant$update(xydt = xydt, y_cols = "y")
  expect_double(surrogate_constant$insample_performance, lower =  -Inf, upper = 1, any.missing = FALSE, len = 1L)
  expect_true(surrogate_constant$insample_performance == 0)
  expect_false(surrogate_constant$test_insample_performance)
  expect_equal(names(surrogate$test_insample_performance), surrogate$param_set$values$performance_measure$id)
})

