test_that("AcqFunction API works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_KM_DETERM, archive = inst$archive)

  acqf = AcqFunction$new(id = "acqf", constants = ParamSet$new(), surrogate = surrogate, direction = "same")

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), "acqf")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "same")
  expect_equal(acqf$domain, inst$search_space)
})
