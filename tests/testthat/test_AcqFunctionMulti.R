test_that("AcqFunctionMulti works for single-objective instances", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqfs = list(AcqFunctionMean$new(), AcqFunctionSD$new())
  acqf_ids = map_chr(acqfs, function(acqf) acqf$id)
  acqf = AcqFunctionMulti$new(acqfs, surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf_ids)
  expect_true(acqf$codomain$length == 2L)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$learner)
  expect_true(acqf$requires_predict_type_se)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 2L, nrows = 5L, any.missing = FALSE)
  expect_named(res, acqf_ids)
})

test_that("AcqFunctionMulti works for multi-objective instances", {
  skip_if_not_installed("emoa")
  skip_if_not_installed("fastGHQuad")
  inst = MAKE_INST(OBJ_1D_2, PS_1D, trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst$archive)
  acqfs = list(AcqFunctionEHVI$new(), AcqFunctionEHVIGH$new())
  acqf_ids = map_chr(acqfs, function(acqf) acqf$id)
  acqf = AcqFunctionMulti$new(acqfs, surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf_ids)
  expect_true(acqf$codomain$length == 2L)
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
  expect_data_table(res, ncols = 2L, nrows = 5L, any.missing = FALSE)
  expect_named(res, acqf_ids)
})

test_that("AcqFunctionMulti API works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqfs = list(AcqFunctionMean$new(), AcqFunctionSD$new())
  acqf_ids = map_chr(acqfs, function(acqf) acqf$id)
  acqf = AcqFunctionMulti$new(acqfs, surrogate = surrogate)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf_ids)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$learner)
  expect_true(acqf$requires_predict_type_se)
  expect_equal(acqf$packages, NULL)
  expect_true(grepl("Multi Acquisition Function of", x = acqf$label))
  expect_true(grepl("mlr_acqfunctions_multi", x = acqf$man))
})

test_that("AcqFunctionMulti lazy initialization", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)

  # no surrogate of acquisition functions, no surrogate during initialization
  acqfs = list(AcqFunctionMean$new(), AcqFunctionSD$new())
  acqf = AcqFunctionMulti$new(acqfs)
  expect_acqfunction(acqf)
  expect_true(acqf$domain$length == 0L)
  expect_true(acqf$codomain$length == 0L)
  expect_true(acqfs[[1L]]$domain$length == 0L)
  expect_true(acqfs[[1L]]$codomain$length == 0L)
  expect_true(acqfs[[2L]]$domain$length == 0L)
  expect_true(acqfs[[2L]]$codomain$length == 0L)

  # lazy initialization of individual acquisition functions will result in an error during updating
  acqf$acq_functions[[1L]]$surrogate = surrogate
  expect_error(acqf$update(), "Acquisition functions must rely on the same surrogate model")

  # lazy initialization
  acqf$surrogate = surrogate
  expect_true(acqf$domain$length == 1L)
  expect_true(acqf$codomain$length == 2L)
  expect_true(acqfs[[1L]]$codomain$length == 1L)
  expect_true(acqfs[[2L]]$codomain$length == 1L)
  expect_equal(address(acqf$surrogate), address(acqfs[[1L]]$surrogate))
  expect_equal(address(acqf$surrogate), address(acqfs[[2L]]$surrogate))
  expect_equal(acqf$archive, acqfs[[1L]]$archive)
  expect_equal(acqf$archive, acqfs[[2L]]$archive)
  expect_equal(acqf$domain, acqfs[[1L]]$domain)
  expect_equal(acqf$domain, acqfs[[2L]]$domain)

  # no surrogate of acquisition functions, surrogate during initialization
  acqfs = list(AcqFunctionMean$new(), AcqFunctionSD$new())
  acqf = AcqFunctionMulti$new(acqfs, surrogate = surrogate)
  expect_acqfunction(acqf)
  expect_true(acqf$domain$length == 1L)
  expect_true(acqf$codomain$length == 2L)
  expect_true(acqfs[[1L]]$codomain$length == 1L)
  expect_true(acqfs[[2L]]$codomain$length == 1L)
  expect_equal(address(acqf$surrogate), address(acqfs[[1L]]$surrogate))
  expect_equal(address(acqf$surrogate), address(acqfs[[2L]]$surrogate))
  expect_equal(acqf$archive, acqfs[[1L]]$archive)
  expect_equal(acqf$archive, acqfs[[2L]]$archive)
  expect_equal(acqf$domain, acqfs[[1L]]$domain)
  expect_equal(acqf$domain, acqfs[[2L]]$domain)

  # surrogates of acquisition functions, no surrogate during initialization
  acqfs = list(AcqFunctionMean$new(surrogate), AcqFunctionSD$new(surrogate))
  acqf = AcqFunctionMulti$new(acqfs)
  expect_acqfunction(acqf)
  expect_true(acqf$domain$length == 1L)
  expect_true(acqf$codomain$length == 2L)
  expect_true(acqfs[[1L]]$codomain$length == 1L)
  expect_true(acqfs[[2L]]$codomain$length == 1L)
  expect_equal(address(acqf$surrogate), address(acqfs[[1L]]$surrogate))
  expect_equal(address(acqf$surrogate), address(acqfs[[2L]]$surrogate))
  expect_equal(acqf$archive, acqfs[[1L]]$archive)
  expect_equal(acqf$archive, acqfs[[2L]]$archive)
  expect_equal(acqf$domain, acqfs[[1L]]$domain)
  expect_equal(acqf$domain, acqfs[[2L]]$domain)

  # surrogates of acquisition functions, different surrogate during initialization
  acqfs = list(AcqFunctionMean$new(surrogate), AcqFunctionSD$new(surrogate))
  surrogate_acqf = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqf = AcqFunctionMulti$new(acqfs)
  expect_acqfunction(acqf)
  expect_true(acqf$domain$length == 1L)
  expect_true(acqf$codomain$length == 2L)
  expect_true(acqfs[[1L]]$codomain$length == 1L)
  expect_true(acqfs[[2L]]$codomain$length == 1L)
  expect_equal(address(acqf$surrogate), address(acqfs[[1L]]$surrogate))
  expect_equal(address(acqf$surrogate), address(acqfs[[2L]]$surrogate))
  expect_equal(acqf$archive, acqfs[[1L]]$archive)
  expect_equal(acqf$archive, acqfs[[2L]]$archive)
  expect_equal(acqf$domain, acqfs[[1L]]$domain)
  expect_equal(acqf$domain, acqfs[[2L]]$domain)

  # surrogate during initialization but no archive will error
  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  acqfs = list(AcqFunctionMean$new(), AcqFunctionSD$new())
  expect_error(AcqFunctionMulti$new(acqfs, surrogate = surrogate), "Must be an R6 class, not 'NULL'")

  # different surrogates of acquisition functions will error
  surrogate_mean = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  surrogate_sd = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  acqfs = list(AcqFunctionMean$new(surrogate_mean), AcqFunctionSD$new(surrogate_sd))
  expect_error(AcqFunctionMulti$new(acqfs), "Acquisition functions must rely on the same surrogate model")
})

