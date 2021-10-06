test_that("AcqFunctionSmsEgo works", {
  surrogate = SURR2D_KM_DETERM
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf = AcqFunctionSmsEgo$new(surrogate = surrogate)
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y1 = 1, y2 = 1))
  expect_equal(acqf$direction, "minimize")
  expect_equal(acqf$domain, inst$search_space)
  # FIXME: expect_learner(acqf$surrogate$model)

  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y)  # update surrogate model with new data

  xdt = data.table(x = 1:5)

  # FIXME: expect_error(acqf$eval_dt(xdt), "update")
  acqf$progress = 1  # FIXME:
  acqf$update(inst$archive)
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_sms_ego")
})

