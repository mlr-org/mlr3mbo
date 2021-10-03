test_that("SurrogateMultiCritLearners API works", {
  surrogate = SurrogateMultiCritLearners$new(learner = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D_2$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = c("y1", "y2"))
  expect_learner(surrogate$model[[1L]])
  expect_learner(surrogate$model[[2L]])

  xdt = data.table(x = seq(5))
  pred = surrogate$predict(xdt)
  expect_list(pred, len = 2L)
  expect_data_table(pred[[1L]], col.names = "named", nrows = 5, ncols = 2,
    any.missing = FALSE)
  expect_data_table(pred[[2L]], col.names = "named", nrows = 5, ncols = 2,
    any.missing = FALSE)
  expect_named(pred[[1L]], c("mean", "se"))
  expect_named(pred[[2L]], c("mean", "se"))

  ### upgrading error class works
  expect_error(surrogate$update(xydt = xydt, y_cols = "y"),
    regexp = "Assertion on 'y_cols' failed: Must be a subset of set \\{x,y1,y2\\}.",
    class = c("leads_to_exploration_error", "update_error"))
})

test_that("predict_types are recognized", {
  surrogate = SurrogateMultiCritLearners$new(learner = list(REGR_KM_DETERM, lrn("regr.rpart")))
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D_2$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = c("y1", "y2"))

  xdt = data.table(x = seq(5))
  pred = surrogate$predict(xdt)
  expect_named(pred[[1L]], c("mean", "se"))
  expect_named(pred[[2L]], "mean")
})

test_that("param_set", {
  surrogate = SurrogateMultiCritLearners$new(learner = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("calc_insample_perf", "perf_measures", "perf_thresholds"))
  expect_r6(surrogate$param_set$params$calc_insample_perf, "ParamLgl")
  expect_r6(surrogate$param_set$params$perf_measure, "ParamUty")
  expect_r6(surrogate$param_set$params$perf_threshold, "ParamUty")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")

})

test_that("insample_perf", {
  set.seed(1)
  surrogate = SurrogateMultiCritLearners$new(learner = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  expect_error({surrogate$insample_perf = c(0, 0)}, regexp = "Field/Binding is read-only.")
  expect_error({surrogate$assert_insample_perf = 0}, regexp = "Field/Binding is read-only.")
  design = generate_design_lhs(PS_1D, 4)$data
  xydt = cbind(design, OBJ_1D_2$eval_dt(design))

  surrogate$update(xydt = xydt, y_cols = c("y1", "y2"))
  expect_equal(surrogate$insample_perf, NaN)

  surrogate$param_set$values$calc_insample_perf = TRUE
  surrogate$update(xydt = xydt, y_cols = c("y1", "y2"))
  expect_double(surrogate$insample_perf, lower = -Inf, upper = 1, any.missing = FALSE, len = 2L)
  expect_equal(names(surrogate$insample_perf), map_chr(surrogate$param_set$values$perf_measures, "id"))

  surrogate_constant = SurrogateMultiCritLearners$new(learner = list(lrn("regr.featureless"), lrn("regr.featureless")))
  surrogate_constant$param_set$values$calc_insample_perf = TRUE
  surrogate_constant$param_set$values$perf_thresholds = c(0.5, 0.5)
  expect_error(surrogate_constant$update(xydt = xydt, y_cols = c("y1", "y2")), regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  expect_double(surrogate_constant$insample_perf, lower = -Inf, upper = 1, any.missing = FALSE, len = 2L)
  expect_true(all(surrogate_constant$insample_perf <= 1e-3))
  expect_equal(names(surrogate$insample_perf), map_chr(surrogate$param_set$values$perf_measures, "id"))
})

test_that("unique in memory", {
  learner = lrn("regr.ranger")
  expect_error(SurrogateMultiCritLearners$new(learners = list(learner, learner)), "Redundant Learners")
})

