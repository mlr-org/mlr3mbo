test_that("AcqFunctionMean works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqf = AcqFunctionMean$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "same")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model)
  expect_false(acqf$requires_predict_type_se)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, acqf$id)
})

