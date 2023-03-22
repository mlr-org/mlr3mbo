test_that("OptimizerMbo works", {
  skip_on_cran()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
  
  optimizer = OptimizerMbo$new()
  expect_r6(optimizer, classes = "OptimizerMbo")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))

  design = MAKE_DESIGN(instance, n = 4L)
  instance$eval_batch(design)

  res = optimizer$optimize(instance)
  expect_equal(res, instance$result)

  opdf = instance$archive$data
  expect_data_table(opdf, any.missing = TRUE, nrows = 5L)
  expect_data_table(tail(opdf, - nrow(design)), any.missing = FALSE, nrows = 5L - nrow(design))
  #expect_equal(instance$result$y, 0, tolerance = 0.1)

  optimizer$optimize(instance)
})

test_that("OptimizerMbo works with different settings - singlecrit", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  # ego x cb,ei,pi x rs x default_surrogate

  # define combinations
  loop_functions = list(
    ego = list(fun = bayesopt_ego)
  )

  acq_functions = list(
    cb = AcqFunctionCB$new(),
    ei = AcqFunctionEI$new(),
    probi = AcqFunctionPI$new()
  )

  acq_optimizers = list(
    rs = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  )

  combinations = cross_join(list(
    loop_function = loop_functions,
    acq_function = acq_functions,
    acq_optimizer = acq_optimizers
  ), sorted = FALSE)

  for (i in seq_row(combinations)) {
    mbofun = combinations[i, "loop_function"][[1L]][[1L]]$fun
    acqfun = combinations[i, "acq_function"][[1L]][[1L]]
    acqopt = combinations[i, "acq_optimizer"][[1L]][[1L]]
    optimizer = OptimizerMbo$new(
      loop_function = mbofun,
      acq_function = acqfun,
      acq_optimizer = acqopt,
    )

    instance = MAKE_INST_1D()

    optimizer$optimize(instance)
    expect_true(any(instance$archive$data$batch_nr > 1L))
  }
})

test_that("OptimizerMbo works for noisy problems", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

 instance = MAKE_INST_1D_NOISY(terminator = trm("evals", n_evals = 5L))

  optimizer = OptimizerMbo$new(
    loop_function = bayesopt_ego,
    surrogate = SurrogateLearner$new(REGR_KM_NOISY),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L)),
    result_assigner = ResultAssignerSurrogate$new(surrogate = SurrogateLearner$new(REGR_KM_NOISY))
  )

  design = MAKE_DESIGN(instance, 4L)
  instance$eval_batch(design)

  optimizer$optimize(instance)

  opdf = instance$archive$data
  expect_data_table(tail(opdf, -nrow(design)), any.missing = FALSE, nrows = 5L - nrow(design))
})

test_that("OptimizerMbo sugar", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  result = bb_optimize(
    OBJ_1D,
    method = opt("mbo", acq_function = acqf("cb"), acq_optimizer = acqo(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))),
    max_evals = 5L
  )

  expect_true(NROW(result$instance$archive$data) == 5L)
  expect_true("acq_cb" %in% colnames(result$instance$archive$data))
})

test_that("OptimizerMbo param_classes", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  optimizer = opt("mbo")
  expect_equal(optimizer$param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"))
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  optimizer$surrogate = default_surrogate(instance)
  expect_equal(optimizer$param_classes, c("ParamLgl", "ParamInt", "ParamDbl"))
  optimizer$acq_optimizer = AcqOptimizer$new(opt("nloptr"), terminator = trm("evals", n_evals = 2L))
  expect_equal(optimizer$param_classes, "ParamDbl")
})

test_that("OptimizerMbo properties", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  optimizer = opt("mbo")
  expect_equal(optimizer$properties, c("dependencies", "single-crit", "multi-crit"))
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  optimizer$surrogate = default_surrogate(instance)
  expect_equal(optimizer$properties, c("single-crit", "multi-crit"))
  optimizer$loop_function = bayesopt_ego
  expect_equal(optimizer$properties, "single-crit")
})

test_that("OptimizerMbo packages", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
  skip_if_not_installed("nloptr")
  skip_if_not_installed("ranger")

  optimizer = opt("mbo")
  expect_equal(optimizer$packages, "mlr3mbo")
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  optimizer$surrogate = default_surrogate(instance)
  expect_equal(optimizer$packages, c("mlr3mbo", "mlr3", "mlr3learners", "DiceKriging"))
  optimizer$acq_optimizer = AcqOptimizer$new(opt("nloptr"), terminator = trm("evals", n_evals = 2L))
  expect_equal(optimizer$packages, c("mlr3mbo", "mlr3", "mlr3learners", "DiceKriging", "bbotk", "nloptr"))
  optimizer$surrogate = SurrogateLearner$new(lrn("regr.ranger"))
  expect_equal(optimizer$packages, c("mlr3mbo", "mlr3", "mlr3learners", "ranger", "bbotk", "nloptr"))
})

test_that("OptimizerMbo args", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")

  optimizer = opt("mbo", args = list(test = 1))
  expect_equal(optimizer$args, list(test = 1))
  optimizer$loop_function = bayesopt_ego
  expect_error(optimizer$args, "Must be a subset of \\{'init_design_size','random_interleave_iter'\\}, but has additional elements \\{'test'\\}.")
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  expect_error(optimizer$optimize(instance), "Must be a subset of \\{'init_design_size','random_interleave_iter'\\}, but has additional elements \\{'test'\\}.")
  expect_equal(instance$archive$data, data.table())
  optimizer$args = list(random_interleave_iter = 1L)
  optimizer$optimize(instance)
  expect_equal(nrow(instance$archive$data), 5L)
  expect_true(optimizer$acq_function$id %nin% colnames(instance$archive$data))
})

test_that("OptimizerMbo reset", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")

  optimizer = opt("mbo", acq_optimizer = acqo(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L)))
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  optimizer$optimize(instance)

  instance_mult = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))

  expect_error(optimizer$optimize(instance_mult), "does not support multi-crit objectives")
  expect_loop_function(optimizer$loop_function)
  expect_r6(optimizer$surrogate, "Surrogate")
  expect_r6(optimizer$acq_function, "AcqFunction")
  expect_r6(optimizer$acq_optimizer, "AcqOptimizer")

  optimizer$reset()
  expect_null(optimizer$loop_function)
  expect_null(optimizer$surrogate)
  expect_null(optimizer$acq_function)
  expect_null(optimizer$acq_optimizer)

  optimizer$optimize(instance_mult)
  expect_loop_function(optimizer$loop_function)
  expect_r6(optimizer$surrogate, "Surrogate")
  expect_r6(optimizer$acq_function, "AcqFunction")
  expect_r6(optimizer$acq_optimizer, "AcqOptimizer")
})

