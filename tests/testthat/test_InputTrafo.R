test_that("InputTrafo API works", {
  it = InputTrafo$new()
  expect_r6(it, "InputTrafo")
})

