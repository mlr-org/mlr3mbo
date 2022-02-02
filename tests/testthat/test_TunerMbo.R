test_that("TunerMbo works", {
  withr::local_seed(1)
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

test_that("Constructing TunerMbo and ABs", {
  withr::local_seed(1)
  tuner = tnr("mbo")
  expect_r6(tuner, classes = "TunerMbo")
  expect_r6(tuner$.__enclos_env__$private$.optimizer, classes = "OptimizerMbo")

  learner = lrn("classif.debug", x = to_tune())

  instance = TuningInstanceSingleCrit$new(tsk("pima"), learner = learner, resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("evals", n_evals = 5L))

  surrogate = default_surrogate(instance)
  tuner = tnr("mbo", loop_function = bayesopt_ego, surrogate = surrogate, acq_function = AcqFunctionEI$new(), acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L)))

  optimizer = tuner[[".__enclos_env__"]][["private"]][[".optimizer"]]

  expect_identical(tuner$loop_function, optimizer$loop_function)
  expect_identical(tuner$surrogate, optimizer$surrogate)
  expect_identical(tuner$acq_function, optimizer$acq_function)
  expect_identical(tuner$acq_optimizer, optimizer$acq_optimizer)
  expect_identical(tuner$args, optimizer$args)
  expect_identical(tuner$result_function, optimizer$result_function)

  tuner$optimize(instance)

  expect_identical(tuner$loop_function, optimizer$loop_function)
  expect_identical(tuner$surrogate, optimizer$surrogate)
  expect_identical(tuner$acq_function, optimizer$acq_function)
  expect_identical(tuner$acq_optimizer, optimizer$acq_optimizer)
  expect_identical(tuner$args, optimizer$args)
  expect_identical(tuner$result_function, optimizer$result_function)
})

test_that("TunerMbo sugar", {
  withr::local_seed(1)
  learner = lrn("classif.debug", x = to_tune())

  instance = tune(
    method = "mbo",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    term_evals = 5L,
    acq_function = acqf("cb")
  )

  expect_true(NROW(instance$archive$data) == 5L)
  expect_true("acq_cb" %in% colnames(instance$archive$data))
})

