test_that("SurrogateLearnerCollection API works", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst$archive)
  expect_r6(surrogate$archive, "Archive")
  expect_equal(surrogate$cols_x, "x")
  expect_equal(surrogate$cols_y, c("y1", "y2"))
  surrogate$update()
  expect_learner(surrogate$learner[[1L]])
  expect_learner(surrogate$learner[[2L]])

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_list(pred, len = 2L)
  expect_data_table(pred[[1L]], col.names = "named", nrows = 5L, ncols = 2L, any.missing = FALSE)
  expect_data_table(pred[[2L]], col.names = "named", nrows = 5L, ncols = 2L, any.missing = FALSE)
  expect_named(pred[[1L]], c("mean", "se"))
  expect_named(pred[[2L]], c("mean", "se"))

  # upgrading error class works
  surrogate = SurrogateLearnerCollection$new(learners = list(LearnerRegrError$new(), LearnerRegrError$new()), archive = inst$archive)
  expect_error(surrogate$update(), class = "surrogate_update_error")

  surrogate$param_set$values$catch_errors = FALSE
  expect_error(surrogate$optimize(), class = "simpleError")

  # predict_type
  expect_equal(surrogate$predict_type, surrogate$learner[[1L]]$predict_type)
  expect_equal(surrogate$predict_type, surrogate$learner[[2L]]$predict_type)
  surrogate$learner[[1L]]$predict_type = "response"
  expect_error({surrogate$predict_type}, "Learners have different active predict types")
  surrogate$learner[[2L]]$predict_type = "response"
  expect_equal(surrogate$predict_type, surrogate$learner[[1L]]$predict_type)
  expect_equal(surrogate$predict_type, surrogate$learner[[2L]]$predict_type)
  expect_error({surrogate$predict_type = "response"}, "is read-only")
})

test_that("predict_types are recognized", {
  skip_if_not_installed("rpart")
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  learner1 = REGR_FEATURELESS$clone(deep = TRUE)
  learner1$predict_type = "se"
  learner2 = lrn("regr.rpart")
  learner2$predict_type = "response"
  surrogate = SurrogateLearnerCollection$new(learners = list(learner1, learner2), archive = inst$archive)
  surrogate$update()

  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  pred = surrogate$predict(xdt)
  expect_named(pred[[1L]], c("mean", "se"))
  expect_named(pred[[2L]], "mean")
})

test_that("param_set", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst$archive)
  expect_r6(surrogate$param_set, "ParamSet")
  expect_setequal(surrogate$param_set$ids(), c("catch_errors", "impute_method"))
  expect_equal(surrogate$param_set$class[["catch_errors"]], "ParamLgl")
  expect_equal(surrogate$param_set$class[["impute_method"]], "ParamFct")
  expect_error({surrogate$param_set = list()}, regexp = "param_set is read-only.")
})

test_that("unique in memory", {
  learner = REGR_FEATURELESS
  expect_error(SurrogateLearnerCollection$new(learners = list(learner, learner)), "Redundant Learners")
})

test_that("deep clone", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate1 = SurrogateLearnerCollection$new(learners = list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst$archive)
  surrogate2 = surrogate1$clone(deep = TRUE)
  expect_true(address(surrogate1) != address(surrogate2))
  expect_true(address(surrogate1$learner) != address(surrogate2$learner))
  expect_true(address(surrogate1$archive) != address(surrogate2$archive))
  inst$eval_batch(MAKE_DESIGN(inst))
  expect_true(address(surrogate1$archive$data) != address(surrogate2$archive$data))
})

test_that("packages", {
  skip_if_missing_regr_km()
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_KM_DETERM, REGR_FEATURELESS))
  expect_equal(surrogate$packages, unique(unlist(map(surrogate$learner, "packages"))))
})

test_that("feature types", {
  skip_if_missing_regr_km()
  surrogate = SurrogateLearnerCollection$new(learners = list(REGR_KM_DETERM, REGR_FEATURELESS))
  expect_equal(surrogate$feature_types, Reduce(intersect, map(surrogate$learner, "feature_types")))
})

