test_that("AcqFunction API works", {
  surrogate = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
  design = generate_design_lhs(OBJ_1D$domain, 4)$data
  inst = MAKE_INST_1D(terminator = trm("evals", n_evals = 5))
  inst$eval_batch(design)

  acqf = AcqFunction$new(id = "acqf", param_set = ParamSet$new(),
    surrogate = surrogate, direction = "same")
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "same")
  expect_equal(acqf$search_space, inst$search_space)
  expect_r6(acqf$generate_objective(), "ObjectiveRFunDt")
})
