test_that("AcqFunctionAEI works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_KM_DETERM, archive = inst$archive)
  acqf = AcqFunctionAEI$new(surrogate = surrogate)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), "acq_aei")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model)

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

