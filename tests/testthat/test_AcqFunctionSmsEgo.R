test_that("AcqFunctionSmsEgo works", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)), archive = inst$archive)
  acqf = AcqFunctionSmsEgo$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), "acq_sms_ego")
  expect_equal(acqf$surrogate_max_to_min, c(y1 = 1, y2 = 1))
  expect_equal(acqf$direction, "minimize")
  expect_equal(acqf$domain, inst$search_space)
  expect_list(acqf$surrogate$model, types = "Learner")

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_error(acqf$eval_dt(xdt), "update")
  expect_error(acqf$update(), "progress")
  acqf$progress = 1
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_sms_ego")
})

