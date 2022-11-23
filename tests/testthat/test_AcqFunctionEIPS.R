test_that("AcqFunctionEIPS works", {
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2, time = xdt$x + 10),
    domain = PS_1D,
    codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time"))
  )
  inst = MAKE_INST(objective = objective, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst$archive, y_cols = c("y", "time"))
  acqf = AcqFunctionEIPS$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1, time = 1))  # FIXME: check this
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  # FIXME: expect_learner(acqf$surrogate$model)

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

