test_that("AcqFunction API works", {
  surrogate = SURR_KM_DETERM
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf = AcqFunction$new(id = "acqf", constants = ParamSet$new(),
    surrogate = surrogate, direction = "same", fun = function(xdt) 0)
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "same")
  expect_equal(acqf$domain, inst$search_space)
})
