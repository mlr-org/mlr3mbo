test_that("AcqFunctionMES works", {
  surrogate = SURR_KM_DETERM
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf = AcqFunctionMES$new(surrogate = surrogate)
  acqf$setup(inst$archive)
  expect_data_table(acqf$grid, nrows = 10000L, ncol = 1L)

  expect_r6(acqf$codomain, "ParamSet")
  expect_r6(acqf$param_set, "ParamSet")
  expect_equal(acqf$param_set$ids(), c("grid_size", "n_maxes"))
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model)

  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y) # update surrogate model with new data

  xdt = data.table(x = seq(5))

  expect_error(acqf$eval_dt(xdt), "update")

  acqf$update(inst$archive)
  expect_numeric(acqf$maxes, finite = TRUE, any.missing = FALSE, len = acqf$param_set$values$n_maxes)
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_mes")
})

