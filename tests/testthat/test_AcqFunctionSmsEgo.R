test_that("AcqFunctionSmsEgo works", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  acqf = AcqFunctionSmsEgo$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y1 = 1, y2 = 1))
  expect_equal(acqf$direction, "minimize")
  expect_equal(acqf$domain, inst$search_space)
  expect_list(acqf$surrogate$learner, types = "Learner")
  expect_true(acqf$requires_predict_type_se)

  expect_r6(acqf$constants, "ParamSet")
  expect_equal(acqf$constants$ids(), c("lambda", "epsilon"))

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  expect_error(acqf$eval_dt(xdt), "update")
  expect_error(acqf$update(), "progress")
  acqf$progress = 1
  acqf$update()
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 2L, nrows = 5L, any.missing = FALSE)
  expect_named(res)
  expect_setequal(colnames(res), c(acqf$id, "acq_epsilon"))
})

test_that("AcqFunctionSmsEgo transforms ys_front onto the output trafo scale", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  ot = OutputTrafoStandardize$new()
  ot$invert_posterior = FALSE
  surrogate$output_trafo = ot
  acqf = AcqFunctionSmsEgo$new(surrogate = surrogate)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  expect_true(surrogate$output_trafo_must_be_considered)
  acqf$progress = 1
  acqf$update()

  expected = ot$transform(inst$archive$best()[, inst$archive$cols_y, with = FALSE])
  for (col in inst$archive$cols_y) {
    set(expected, j = col, value = expected[[col]] * acqf$surrogate_max_to_min[[col]])
  }
  expect_equal(acqf$ys_front, as.matrix(expected))
  expect_true(all(apply(acqf$ys_front, 2L, max) <= acqf$ref_point))
})
