context("AcqFunction")

library(mlr3learners)

test_that("AcqFunction API works", {
  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  design = generate_design_lhs(OBJ_1D$domain, 4)$data
  inst = MAKE_INST_1D(terminator = trm("evals", n_evals = 5))
  inst$eval_batch(design)

  acqf = AcqFunction$new(id = "acqf", param_set = ParamSet$new(),
    surrogate = surrogate, direction = "same")
  acqf$setup(inst$archive)

  expect_r6(acqf$generate_objective(), "ObjectiveRFunDt")
})
