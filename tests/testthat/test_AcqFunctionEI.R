test_that("AcqFunctionEI works", {
  surrogate = SURR_KM_DETERM
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf = AcqFunctionEI$new(surrogate = surrogate)
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model)

  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y)  # update surrogate model with new data

  xdt = data.table(x = 1:5)

  expect_error(acqf$eval_dt(xdt), "update")

  acqf$update(inst$archive)
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_ei")
})

test_that("AcqFunctionEI trafo", {
  set.seed(1)
  domain = ps(x = p_dbl(lower = 10, upper = 20, trafo = function(x) x - 15))
  obj = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2),
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )
  inst = MAKE_INST(objective = obj, search_space = domain, terminator = trm("evals", n_evals = 5L))
  design = data.table(x = c(10, 14, 16, 20))
  inst$eval_batch(design)

  acqf = AcqFunctionEI$new(surrogate = SURR_KM_DETERM)
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_true(!acqf$domain$has_trafo)

  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y)  # update surrogate model with new data

  xdt = data.table(x = 10:20)
  acqf$update(inst$archive)
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 11L, any.missing = FALSE)
  expect_named(res, "acq_ei")
  expect_true(xdt[which.max(res$acq_ei)]$x == 15)
})

