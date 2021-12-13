test_that("AcqFunctionEHVI works", {
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y1 = xdt$x^2, y2 = (xdt$x - 2) ^ 2),
    domain = ps(x = p_dbl(lower = -10, upper = 10)),
    codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
  )
  inst = OptimInstanceMultiCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 1000L)
  )
  surrogate = SurrogateLearners$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)), archive = inst$archive)
  acqf = AcqFunctionEHVI$new(surrogate = surrogate)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), "acq_ehvi")
  expect_equal(acqf$surrogate_max_to_min, c(y1 = 1, y2 = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  # FIXME: expect_learner(acqf$surrogate$model)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-10, 10, length.out = 5L))
  expect_error(acqf$eval_dt(xdt), "update")
  acqf$update()
  res = acqf$eval_dt(xdt)  # FIXME: check this
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, "acq_ehvi")
})

