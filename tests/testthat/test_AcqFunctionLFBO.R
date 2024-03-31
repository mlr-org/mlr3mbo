test_that("AcqFunctionLFBO works", {
  skip()
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(lrn("regr.lfbo", learner_classif = lrn("classif.rpart"), lfbo.direction = "minimize"), archive = inst$archive)
  acqf = AcqFunctionLFBO$new(surrogate = surrogate)
  expect_acqfunction(acqf)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), acqf$id)
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "maximize")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$model)
  expect_learner(acqf$surrogate$model$learner_classif)

  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf$surrogate$update()
  xdt = data.table(x = seq(-1, 1, length.out = 5L))
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1L, nrows = 5L, any.missing = FALSE)
  expect_named(res, acqf$id)
})

