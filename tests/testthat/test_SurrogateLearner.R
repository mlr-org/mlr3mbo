test_that("SurrogateLearner API works", {
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)
  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM, archive = inst$archive)
  expect_r6(surrogate$archive, "Archive")
  expect_equal(surrogate$y_cols, "y")
  surrogate$update()
  expect_learner(surrogate$model)

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_data_table(pred, col.names = "named", nrows = 5, ncols = 2, any.missing = FALSE)
  expect_named(pred, c("mean", "se"))

  ### upgrading error class works
  # FIXME:
})

test_that("predict_types are recognized", {
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM, archive = inst$archive)
  surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_named(surrogate$predict(xdt), c("mean", "se"))

  surrogate = SurrogateLearner$new(learner = lrn("regr.rpart"), archive = inst$archive)
  surrogate$update()
  expect_named(surrogate$predict(xdt), "mean")
})

test_that("param_set", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM, archive = inst$archive)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("calc_insample_perf", "perf_measure", "perf_threshold"))
  expect_r6(surrogate$param_set$params$calc_insample_perf, "ParamLgl")
  expect_r6(surrogate$param_set$params$perf_measure, "ParamUty")
  expect_r6(surrogate$param_set$params$perf_threshold, "ParamDbl")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")
})

test_that("insample_perf", {
  withr::local_seed(1)
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  surrogate = SurrogateLearner$new(learner = REGR_KM_DETERM, archive = inst$archive)
  expect_error({surrogate$insample_perf = 0}, regexp = "insample_perf is read-only.")
  expect_error({surrogate$assert_insample_perf = 0}, regexp = "assert_insample_perf is read-only.")

  surrogate$update()
  expect_equal(surrogate$insample_perf, NaN)

  surrogate$param_set$values$calc_insample_perf = TRUE
  surrogate$update()
  expect_number(surrogate$insample_perf, lower = -Inf, upper = 1)
  expect_equal(names(surrogate$insample_perf), surrogate$param_set$values$perf_measure$id)

  surrogate_constant = SurrogateLearner$new(learner = lrn("regr.featureless"), archive = inst$archive)
  surrogate_constant$param_set$values$calc_insample_perf = TRUE
  surrogate_constant$param_set$values$perf_threshold = 0.5
  expect_error(surrogate_constant$update(), regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  expect_number(surrogate_constant$insample_perf, lower = -Inf, upper = 1)
  expect_true(surrogate_constant$insample_perf <= 1e-3)
  expect_equal(names(surrogate$insample_perf), surrogate$param_set$values$perf_measure$id)
})

test_that("deep clone", {
  inst = MAKE_INST_1D()
  surrogate1 = SurrogateLearner$new(learner = REGR_KM_DETERM, archive = inst$archive)
  surrogate2 = surrogate1$clone(deep = TRUE)
  expect_true(address(surrogate1) != address(surrogate2))
  expect_true(address(surrogate1$model) != address(surrogate2$model))
})

