context("SurrogateLearner")

library(mlr3learners)

test_that("SurrogateLearner works", {
  fun = function(x) sqrt(x) * sin(x)
  xdt = data.table(x1 = seq(from = 1, to = 10, by = 0.5))
  ydt = data.table(y = fun(xdt$x1))

  sl = SurrogateLearner$new(lrn("regr.ranger"))
  expect_silent(sl$update(xdt, ydt))

  new_xdt = data.table(x1 = 0.7)
  pred = sl$predict(new_xdt)
  expect_named(pred,c("y", "se"))
  expect_data_table(pred, any.missing = FALSE)
  expect_numeric(pred$y)
  expect_numeric(pred$se)
})
