test_that("OutputTrafo max_to_min setter requires a named vector matching cols_y", {
  ot = OutputTrafoStandardize$new()
  ot$cols_y = "y"
  expect_error({ot$max_to_min = 1L}, "names")
  expect_error({ot$max_to_min = c(z = 1L)}, "permutation")
  expect_error({ot$max_to_min = c(y = 2L)}, "subset")
  ot$max_to_min = c(y = -1L)
  expect_equal(ot$max_to_min, c(y = -1L))
})
