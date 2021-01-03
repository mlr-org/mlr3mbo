test_that("AcqFunctionEIPS works", {
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf = AcqFunctionEIPS$new(surrogate = SURR2D_KM_DETERM)
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model[[1]])
  expect_learner(acqf$surrogate$model[[2]])

  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y) # update surrogate model with new data

  xdt = data.table(x = seq(5))

  expect_error(acqf$eval_dt(xdt), "update")

  acqf$update(inst$archive)
  xdt = data.table(x = seq(5))
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1, nrows = 5, any.missing = FALSE)
  expect_named(res, "acq_eips")
})

