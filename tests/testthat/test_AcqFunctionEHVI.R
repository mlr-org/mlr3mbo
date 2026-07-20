test_that("AcqFunctionEHVI works", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  acqf = AcqFunctionEHVI$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y1 = 1, y2 = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_list(acqf$surrogate$learner, types = "Learner")
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
  expect_true(all(res[[acqf$id]] >= 0))
})

test_that("AcqFunctionEHVI transforms ys_front onto the output trafo scale", {
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  ot = OutputTrafoStandardize$new()
  ot$invert_posterior = FALSE
  surrogate$output_trafo = ot
  acqf = AcqFunctionEHVI$new(surrogate = surrogate)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  expect_true(surrogate$output_trafo_must_be_considered)
  acqf$update()

  expected = ot$transform(inst$archive$best()[, inst$archive$cols_y, with = FALSE])
  for (col in inst$archive$cols_y) {
    set(expected, j = col, value = expected[[col]] * acqf$surrogate_max_to_min[[col]])
  }
  setorderv(expected, cols = inst$archive$cols_y[1L], order = -1L)
  expect_equal(acqf$ys_front, as.matrix(expected))
  # ys_front and ref_point must live on the same (transformed) scale
  expect_true(all(apply(acqf$ys_front, 2L, max) <= acqf$ref_point))
})
