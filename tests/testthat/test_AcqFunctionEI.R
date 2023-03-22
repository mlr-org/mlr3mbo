test_that("AcqFunctionEI works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqf = AcqFunctionEI$new(surrogate = surrogate)
  expect_acqfunction(acqf)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
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
  expect_named(res, acqf$id)
})

test_that("AcqFunctionEI trafo", {
  domain = ps(x = p_dbl(lower = 10, upper = 20, trafo = function(x) x - 15))
  obj = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2),
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )
  inst = MAKE_INST(objective = obj, search_space = domain, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqf = AcqFunctionEI$new(surrogate = surrogate)

  expect_r6(acqf$codomain, "ParamSet")
  expect_true(!acqf$domain$has_trafo)

  design = data.table(x = c(10, 14, 16, 20))
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = 10:20)
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 11L, any.missing = FALSE)
  expect_named(res, acqf$id)
  expect_true(max(res$acq_ei) == res$acq_ei[6])  # at x = 15
})

