test_that("OptimizerMbo works", {

  optimizer = OptimizerMbo$new()
  expect_r6(optimizer, classes = "OptimizerMbo")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))

  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)

  res = optimizer$optimize(instance)
  expect_equal(res, instance$result)

  opdf = instance$archive$data
  expect_data_table(opdf, any.missing = TRUE, nrows = 10L)
  expect_data_table(tail(opdf, - nrow(design)), any.missing = FALSE, nrows = 10L - nrow(design))
  expect_equal(instance$result$y, 0, tolerance = 0.1)

  optimizer$optimize(instance)
})

test_that("OptimizerMbo works with different settings - singlecrit", {
  # ego,mpcl x cb,ei,pi x rs x default_surrogate

  # define combinations
  loop_functions = list(
    ego = list(fun = bayesopt_ego),
    mpcl = list(fun = bayesopt_mpcl)
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

  instance = OptimInstanceSingleCrit$new(
    objective = OBJ_2D_NOISY,
    terminator = trm("evals", n_evals = 20L),
    search_space = PS_2D
  )

  optimizer = OptimizerMbo$new(
    loop_function = bayesopt_ego,
    surrogate = SurrogateLearner$new(REGR_KM_NOISY),
    acq_function = AcqFunctionAEI$new(),
    acq_optimizer = NULL,
    result_function = result_by_surrogate_design,
  )

  design = MAKE_DESIGN(instance, 12L)
  instance$eval_batch(design)

  optimizer$optimize(instance)

  opdf = instance$archive$data
  expect_data_table(tail(opdf, -nrow(design)), any.missing = FALSE, nrows = 20L - nrow(design))
  #FIXME: can we test that the surrogate actually influneces the choice?
  #expect_true(instance$result$y > min(opdf$y)) # we have not chosen the overoptimistic noisy y
  #expect_equal(instance$result$y, 0, tolerance = 0.2)
})

test_that("OptimizerMbo sugar", {
  instance = bb_optimize(
    OBJ_1D,
    method = "mbo",
    max_evals = 5L
    #acq_function = acqf("cb")  # FIXME: https://github.com/mlr-org/bbotk/issues/169
  )

  expect_true(NROW(instance$instance$archive$data) == 5L)  # FIXME: this is somewhat inconsistent to the tune sugar output
  #expect_true("acq_cb" %in% colnames(instance$archive$data))
})

