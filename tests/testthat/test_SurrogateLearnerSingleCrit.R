context("SurrogateSingleCritLearner")

library(mlr3learners)

test_that("SurrogateSingleCritLearner API works", {

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
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
  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
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

