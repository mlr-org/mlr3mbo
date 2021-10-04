test_that("SurrogateSingleCritLearner API works", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = "y")
  expect_learner(surrogate$model)

  xdt = data.table(x = 1:5)
  pred = surrogate$predict(xdt)
  expect_data_table(pred, col.names = "named", nrows = 5, ncols = 2,
    any.missing = FALSE)
  expect_named(pred, c("mean", "se"))

  ### upgrading error class works
  expect_error(surrogate$update(xydt = xydt, y_cols = c("y1", "y2")),
    regexp = "Assertion on 'y_cols' failed: Must be a subset of set \\{x,y\\}.",
    class = c("leads_to_exploration_error", "update_error"))
})

test_that("predict_types are recognized", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = "y")

  xdt = data.table(x = 1:5)
  expect_named(surrogate$predict(xdt), c("mean", "se"))

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.rpart"))
  surrogate$update(xydt = xydt, y_cols = "y")

  surrogate$predict(xdt)
  expect_named(surrogate$predict(xdt), "mean")
})

test_that("param_set", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("calc_insample_perf", "perf_measure", "perf_threshold"))
  expect_r6(surrogate$param_set$params$calc_insample_perf, "ParamLgl")
  expect_r6(surrogate$param_set$params$perf_measure, "ParamUty")
  expect_r6(surrogate$param_set$params$perf_threshold, "ParamDbl")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")

})

test_that("insample_perf", {
  set.seed(1)
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  expect_error({surrogate$insample_perf = 0}, regexp = "Field/Binding is read-only.")
  expect_error({surrogate$assert_insample_perf = 0}, regexp = "Field/Binding is read-only.")
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = "y")
  expect_equal(surrogate$insample_perf, NaN)

  surrogate$param_set$values$calc_insample_perf = TRUE
  surrogate$update(xydt = xydt, y_cols = "y")
  expect_double(surrogate$insample_perf, lower = -Inf, upper = 1, any.missing = FALSE, len = 1L)
  expect_equal(names(surrogate$insample_perf), surrogate$param_set$values$perf_measure$id)

  surrogate_constant = SurrogateSingleCritLearner$new(learner = lrn("regr.featureless"))
  surrogate_constant$param_set$values$calc_insample_perf = TRUE
  surrogate_constant$param_set$values$perf_threshold = 0.5
  expect_error(surrogate_constant$update(xydt = xydt, y_cols = "y"), regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  expect_double(surrogate_constant$insample_perf, lower = -Inf, upper = 1, any.missing = FALSE, len = 1L)
  expect_true(surrogate_constant$insample_perf <= 1e-3)
  expect_equal(names(surrogate$insample_perf), surrogate$param_set$values$perf_measure$id)
})

test_that("deep clone", {
  surrogate1 = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  surrogate2 = surrogate1$clone(deep = TRUE)
  expect_true(address(surrogate1) != address(surrogate2))
  expect_true(address(surrogate1$model) != address(surrogate2$model))
})

