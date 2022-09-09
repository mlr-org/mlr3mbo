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
  expect_identical(tuner$param_classes, optimizer$param_classes)
  expect_identical(tuner$properties, optimizer$properties)
  expect_identical(tuner$packages, optimizer$packages)

  tuner$optimize(instance)

  expect_identical(tuner$loop_function, optimizer$loop_function)
  expect_identical(tuner$surrogate, optimizer$surrogate)
  expect_identical(tuner$acq_function, optimizer$acq_function)
  expect_identical(tuner$acq_optimizer, optimizer$acq_optimizer)
  expect_identical(tuner$args, optimizer$args)
  expect_identical(tuner$result_function, optimizer$result_function)
  expect_identical(tuner$param_classes, optimizer$param_classes)
  expect_identical(tuner$properties, optimizer$properties)
  expect_identical(tuner$packages, optimizer$packages)
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

test_that("TunerMbo param_classes", {
  tuner = tnr("mbo")
  expect_equal(tuner$param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"))
  instance = TuningInstanceSingleCrit$new(tsk("pima"), learner = lrn("classif.debug", x = to_tune()), resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("evals", n_evals = 5L))
  tuner$surrogate = default_surrogate(instance)
  expect_equal(tuner$param_classes, c("ParamLgl", "ParamInt", "ParamDbl"))
  tuner$acq_optimizer = AcqOptimizer$new(opt("nloptr"), terminator = trm("evals", n_evals = 5L))
  expect_equal(tuner$param_classes, "ParamDbl")
})

test_that("TunerMbo properties", {
  tuner = tnr("mbo")
  expect_equal(tuner$properties, c("dependencies", "single-crit", "multi-crit"))
  instance = TuningInstanceSingleCrit$new(tsk("pima"), learner = lrn("classif.debug", x = to_tune()), resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("evals", n_evals = 5L))
  tuner$surrogate = default_surrogate(instance)
  expect_equal(tuner$properties, c("single-crit", "multi-crit"))
  tuner$loop_function = bayesopt_ego
  expect_equal(tuner$properties, "single-crit")
})

test_that("TunerMbo packages", {
  tuner = opt("mbo")
  expect_equal(tuner$packages, "mlr3mbo")
  instance = TuningInstanceSingleCrit$new(tsk("pima"), learner = lrn("classif.debug", x = to_tune()), resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("evals", n_evals = 5L))
  tuner$surrogate = default_surrogate(instance)
  expect_equal(tuner$packages, c("mlr3mbo", "mlr3", "mlr3learners", "DiceKriging"))
  tuner$acq_optimizer = AcqOptimizer$new(opt("nloptr"), terminator = trm("evals", n_evals = 5L))
  expect_equal(tuner$packages, c("mlr3mbo", "mlr3", "mlr3learners", "DiceKriging", "bbotk", "nloptr"))
  tuner$surrogate = SurrogateLearner$new(lrn("regr.ranger"))
  expect_equal(tuner$packages, c("mlr3mbo", "mlr3", "mlr3learners", "ranger", "bbotk", "nloptr"))
})

test_that("TunerMbo args", {
  tuner = tnr("mbo", args = list(test = 1))
  expect_equal(tuner$args, list(test = 1))
  tuner$loop_function = bayesopt_ego
  expect_error(tuner$args, "Must be a subset of \\{'init_design_size','random_interleave_iter'\\}, but has additional elements \\{'test'\\}.")
  instance = TuningInstanceSingleCrit$new(tsk("pima"), learner = lrn("classif.debug", x = to_tune()), resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("evals", n_evals = 5L))
  expect_error(tuner$optimize(instance), "Must be a subset of \\{'init_design_size','random_interleave_iter'\\}, but has additional elements \\{'test'\\}.")
  expect_equal(instance$archive$data, data.table())
  tuner$args = list(random_interleave_iter = 1L)
  tuner$optimize(instance)
  expect_equal(nrow(instance$archive$data), 5L)
  expect_true(tuner$acq_function$id %nin% colnames(instance$archive$data))
})

