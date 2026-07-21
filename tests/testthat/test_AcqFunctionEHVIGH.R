test_that("AcqFunctionEHVIGH works", {
  skip_if_not_installed("emoa")
  skip_if_not_installed("fastGHQuad")
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  acqf = AcqFunctionEHVIGH$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y1 = 1, y2 = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_list(acqf$surrogate$learner, types = "Learner")
  expect_true(acqf$requires_predict_type_se)
  expect_setequal(acqf$packages, c("emoa", "fastGHQuad"))

  expect_r6(acqf$constants, "ParamSet")
  expect_equal(acqf$constants$ids(), c("k", "r"))

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

test_that("AcqFunctionEHVIGH does not vanish for k = 2", {
  skip_if_not_installed("emoa")
  skip_if_not_installed("fastGHQuad")
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  acqf = AcqFunctionEHVIGH$new(surrogate = surrogate, k = 2L)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  acqf$update()
  res = acqf$eval_dt(data.table(x = seq(-1, 1, length.out = 5L)))
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  # for k = 2 all product weights are equal; strict pruning previously removed every node -> identically 0
  expect_true(any(res[[acqf$id]] > 0))
})

test_that("AcqFunctionEHVIGH transforms ys_front onto the output trafo scale", {
  skip_if_not_installed("emoa")
  skip_if_not_installed("fastGHQuad")
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)),
    archive = inst$archive
  )
  ot = OutputTrafoStandardize$new()
  ot$invert_posterior = FALSE
  surrogate$output_trafo = ot
  acqf = AcqFunctionEHVIGH$new(surrogate = surrogate)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  expect_true(surrogate$output_trafo_must_be_considered)
  acqf$update()

  expected = ot$transform(inst$archive$best()[, inst$archive$cols_y, with = FALSE])
  for (col in inst$archive$cols_y) {
    set(expected, j = col, value = expected[[col]] * acqf$surrogate_max_to_min[[col]])
  }
  expect_equal(acqf$ys_front, as.matrix(expected))
  expect_true(all(apply(acqf$ys_front, 2L, max) <= acqf$ref_point))
})

test_that("AcqFunctionEHVIGH is close to AcqFunctionEHVI", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
  skip_if_not_installed("emoa")
  skip_if_not_installed("fastGHQuad")
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  design = data.table(x = c(-0.8, -0.3, 0.6, 0.9))
  inst$eval_batch(design)
  surrogate = SurrogateLearnerCollection$new(
    list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)),
    archive = inst$archive
  )
  surrogate$update()
  xdt = data.table(x = seq(PS_1D$lower, PS_1D$upper, length.out = 101L))

  acqf = AcqFunctionEHVIGH$new(surrogate = surrogate)
  acqf$update()
  res = acqf$eval_dt(xdt)

  acqf_ref = AcqFunctionEHVI$new(surrogate = surrogate)
  acqf_ref$update()
  res_ref = acqf_ref$eval_dt(xdt)

  x = diff(xdt[[1L]])
  x = c(x[1L], x)

  expect_true(sum(x * abs(res - res_ref)) < 0.1)
})
