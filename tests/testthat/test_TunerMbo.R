test_that("TunerMbo works", {

  tuner = TunerMbo$new()
  expect_r6(tuner, classes = "TunerMbo")
  expect_r6(tuner$.__enclos_env__$private$.optimizer, classes = "OptimizerMbo")

  learner = lrn("classif.debug", x = to_tune())

  instance = TuningInstanceSingleCrit$new(tsk("pima"), learner = learner, resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("evals", n_evals = 5L))

  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)

  res = tuner$optimize(instance)
  expect_equal(res, instance$result)

  opdf = instance$archive$data
  expect_data_table(opdf, any.missing = TRUE, nrows = 5L)
  expect_data_table(tail(opdf, - nrow(design)), any.missing = FALSE, nrows = 5L - nrow(design))

  tuner$optimize(instance)
})

# FIXME: also needs more tests regarding passing of surrogate, acq_function, acq_optimizer etc.
