test_that("ResultAssignerSurrogate works", {
  ras = ResultAssignerSurrogate$new()
  expect_null(ras$surrogate)

  inst = MAKE_INST_1D()
  design = generate_design_random(inst$search_space, n = 4L)$data
  inst$eval_batch(design)

  expect_null(inst$result)
  ras$assign_result(inst)
  expect_r6(ras$surrogate, classes = "SurrogateLearner")
  expect_data_table(inst$result, nrows = 1L)
  # FIXME: test multicrit?
})

test_that("ResultAssignerSurrogate result and best can be different", {
  skip_if_not_installed("rpart")
  ras = ResultAssignerSurrogate$new(surrogate = SurrogateLearner$new(lrn("regr.rpart")))

  inst = MAKE_INST_1D()
  design = generate_design_grid(inst$search_space, resolution = 4L)$data
  inst$eval_batch(design)

  expect_null(inst$result)
  ras$assign_result(inst)
  expect_data_table(inst$result, nrows = 1L)
  mean = ras$surrogate$predict(design)$mean
  best_index = which.min(mean)  # first one
  expect_equal(inst$result[[inst$archive$cols_x]], design[best_index, ][[inst$archive$cols_x]])
  expect_equal(inst$result[[inst$archive$cols_y]], inst$archive$data[best_index, ][[inst$archive$cols_y]])
  expect_true(abs(inst$result[[inst$archive$cols_y]] - mean[best_index]) > 1e-3)
  # FIXME: test multicrit?
})

