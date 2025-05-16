test_that("OutputTrafo API works", {
  ot = OutputTrafo$new(invert_posterior = TRUE)
  expect_r6(ot, "OutputTrafo")
})

