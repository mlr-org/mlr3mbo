test_that("AcqFunctionEIPS works", {
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2, time = xdt$x + 10),
    domain = PS_1D,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
  inst = MAKE_INST(objective = objective, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  expect_error(AcqFunctionEIPS$new(surrogate = SURR_KM_DETERM, surrogate_time = SURR_KM_DETERM, time_id = "time"), "Redundant Learners")

  acqf = AcqFunctionEIPS$new(surrogate = SURR_KM_DETERM, surrogate_time = SURR_KM_DETERM$clone(deep = TRUE), time_id = "time")
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model)
  expect_learner(acqf$surrogate_time$model)

  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y)  # update surrogate model with new data
  acqf$surrogate_time$update(xydt = inst$archive$data[, c(inst$archive$cols_x, "time"), with = FALSE], y_cols = "time")  # update surrogate model with new data

  xdt = data.table(x = 1:5)

  expect_error(acqf$eval_dt(xdt), "update")

  acqf$update(inst$archive)
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_eips")
})

