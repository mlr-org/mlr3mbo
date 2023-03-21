test_that("ResultAssignerArchive works", {
  ras = ResultAssignerArchive$new()

  inst = MAKE_INST_1D()
  design = generate_design_random(inst$search_space, n = 4L)$data
  inst$eval_batch(design)

  expect_null(inst$result)
  ras$assign_result(inst)
  expect_data_table(inst$result, nrows = 1L)
  expect_equal(inst$result[[inst$archive$cols_x]], inst$archive$best()[[inst$archive$cols_x]])
  expect_equal(inst$result[[inst$archive$cols_y]], inst$archive$best()[[inst$archive$cols_y]])
})

