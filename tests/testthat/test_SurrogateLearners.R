test_that("SurrogateLearnerCollection API works", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)), archive = inst$archive)
  expect_r6(surrogate$archive, "Archive")
  expect_equal(surrogate$x_cols, "x")
  expect_equal(surrogate$y_cols, c("y1", "y2"))
  surrogate$update()
  expect_learner(surrogate$model[[1L]])
  expect_learner(surrogate$model[[2L]])

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_list(pred, len = 2L)
  expect_data_table(pred[[1L]], col.names = "named", nrows = 5L, ncols = 2L, any.missing = FALSE)
  expect_data_table(pred[[2L]], col.names = "named", nrows = 5L, ncols = 2L, any.missing = FALSE)
  expect_named(pred[[1L]], c("mean", "se"))
  expect_named(pred[[2L]], c("mean", "se"))

  ### upgrading error class works
  # FIXME:
})

test_that("predict_types are recognized", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  surrogate = SurrogateLearnerCollection$new(learner = list(REGR_KM_DETERM, lrn("regr.rpart")), archive = inst$archive)
  surrogate$update()

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_named(pred[[1L]], c("mean", "se"))
  expect_named(pred[[2L]], "mean")
})

test_that("param_set", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(learner = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)), archive = inst$archive)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("calc_insample_perf", "perf_measures", "perf_thresholds"))
  expect_r6(surrogate$param_set$params$calc_insample_perf, "ParamLgl")
  expect_r6(surrogate$param_set$params$perf_measure, "ParamUty")
  expect_r6(surrogate$param_set$params$perf_threshold, "ParamUty")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")
})

test_that("insample_perf", {
  withr::local_seed(1)
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  surrogate = SurrogateLearnerCollection$new(learner = list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)), archive = inst$archive)
  expect_error({surrogate$insample_perf = c(0, 0)}, regexp = "insample_perf is read-only.")
  expect_error({surrogate$assert_insample_perf = 0}, regexp = "assert_insample_perf is read-only.")

  surrogate$update()
  expect_equal(surrogate$insample_perf, NaN)

  surrogate$param_set$values$calc_insample_perf = TRUE
  surrogate$update()
  expect_double(surrogate$insample_perf, lower = -Inf, upper = 1, any.missing = FALSE, len = 2L)
  expect_equal(names(surrogate$insample_perf), map_chr(surrogate$param_set$values$perf_measures, "id"))

  surrogate_constant = SurrogateLearnerCollection$new(learner = list(lrn("regr.featureless"), lrn("regr.featureless")), archive = inst$archive)
  surrogate_constant$param_set$values$calc_insample_perf = TRUE
  surrogate_constant$param_set$values$perf_thresholds = c(0.5, 0.5)
  expect_error(surrogate_constant$update(), regexp = "Current insample performance of the Surrogate Model does not meet the performance threshold")
  expect_double(surrogate_constant$insample_perf, lower = -Inf, upper = 1, any.missing = FALSE, len = 2L)
  expect_true(all(surrogate_constant$insample_perf <= 1e-3))
  expect_equal(names(surrogate$insample_perf), map_chr(surrogate$param_set$values$perf_measures, "id"))
})

test_that("unique in memory", {
  learner = REGR_KM_DETERM
  expect_error(SurrogateLearnerCollection$new(learners = list(learner, learner)), "Redundant Learners")
})

test_that("deep clone", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  learner = REGR_KM_DETERM
  surrogate1 = SurrogateLearnerCollection$new(learners = list(learner, learner$clone(deep = TRUE)), archive = inst$archive)
  surrogate2 = surrogate1$clone(deep = TRUE)
  expect_true(address(surrogate1) != address(surrogate2))
  expect_true(address(surrogate1$model) != address(surrogate2$model))
})

test_that("packages", {
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_KM_DETERM, REGR_FEATURELESS))
  expect_equal(surrogate$packages, unique(unlist(map(surrogate$model, "packages"))))
})

test_that("feature types", {
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_KM_DETERM, REGR_FEATURELESS))
  expect_equal(surrogate$feature_types, unique(unlist(map(surrogate$model, "feature_types"))))
})

