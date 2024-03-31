skip_if_not_installed("mlr3learners")
skip_if_not_installed("DiceKriging")
skip_if_not_installed("rgenoud")

test_that("AcqFunctionAEI works", {
  inst = MAKE_INST_1D_NOISY()
  surrogate = SurrogateLearner$new(REGR_KM_NOISY, archive = inst$archive)
  acqf = AcqFunctionAEI$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), "acq_aei")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$learner)
  expect_true(acqf$requires_predict_type_se)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_error(acqf$eval_dt(xdt), "update")
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_aei")
})

