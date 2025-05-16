test_that("AcqFunctionEILog works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqf = AcqFunctionEILog$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$learner)
  expect_true(acqf$requires_predict_type_se)

  expect_r6(acqf$constants, "ParamSet")
  expect_equal(acqf$constants$ids(), "epsilon")

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_error(acqf$eval_dt(xdt), "update")
  expect_error(acqf$update(), "Must be an R6 class")
  ot = OutputTrafoLog$new()
  acqf$surrogate$output_trafo = ot
  expect_error(acqf$update(), "Must be FALSE")
  acqf$surrogate$output_trafo$invert_posterior = FALSE
  expect_error(acqf$update(), "update")
  acqf$surrogate$update()
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, acqf$id)

  # maximization
  inst = MAKE_INST(OBJ_1D_MAXIMIZE, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  inst$eval_batch(design)
  acqf$surrogate$update()
  acqf$update()
  res_maximize = acqf$eval_dt(xdt)
  expect_equal(res, res_maximize)
})

