test_that("ResultAssigner API works", {
  ras = ResultAssigner$new()
  expect_r6(ras, "ResultAssigner")
})

