test_that("AcqFunctionTTEI works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqf = AcqFunctionTTEI$new(surrogate = surrogate, toplvl_acq_optimizer = acqo(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L)))
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  constants = ps(beta = p_dbl(lower = 0, upper = 1, default = 0.5))
  constants$values$beta = 0.5
  expect_equal(acqf$constants, constants)
  expect_learner(acqf$surrogate$model)
  expect_r6(mlr3misc::get_private(acqf)[[".toplvl_acq_optimizer"]], classes = "AcqOptimizer")

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$constants$values$beta = 1
  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_error(acqf$eval_dt(xdt), "update")
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 2L, nrows = 5L, any.missing = FALSE)
  expect_named(res, c(acqf$id, "acq_ttei_is_ei"))
  expect_true(all(res$acq_ttei_is_ei))

  acqf$constants$values$beta = 0
  acqf$surrogate$update()
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 2L, nrows = 5L, any.missing = FALSE)
  expect_named(res, c(acqf$id, "acq_ttei_is_ei"))
  expect_true(all(!res$acq_ttei_is_ei))

  acqf = AcqFunctionTTEI$new()
  expect_acqfunction(acqf)
  expect_equal(mlr3misc::get_private(acqf)[[".toplvl_acq_optimizer"]], acqo(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 10000L)))
})

