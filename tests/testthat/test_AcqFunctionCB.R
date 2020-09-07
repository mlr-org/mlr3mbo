context("AcqFunctionCB")

library(mlr3learners)

test_that("AcqFunctionCB API works", {
  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  design = generate_design_lhs(OBJ_1D$domain, 4)$data
  inst = MAKE_INST_1D(terminator = trm("evals", n_evals = 5))
  inst$eval_batch(design)

  acqf = AcqFunctionCB$new(surrogate = surrogate)
  acqf$setup(inst$archive)

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "same")
  expect_equal(acqf$search_space, inst$search_space)
  expect_learner(acqf$surrogate$model)

  xydt = inst$archive$data()
  acqf$surrogate$update(xydt = xydt[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE], y_cols = inst$archive$cols_y) #update surrogate model with new data

  xdt = data.table(x = seq(5))
  res = acqf$eval_dt(xdt)
  expect_data_table(res, ncols = 1, nrows = 5, any.missing = FALSE)
  expect_named(res, "acq_cb")

  # FIXME: Should we assert that the surrogate is already trained? Or that
  # $setup() was already called
})

