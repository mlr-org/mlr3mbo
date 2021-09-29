test_that("OptimizerMbo works", {

  optim = OptimizerMbo$new(
    loop_function = bayesopt_soo,
    acq_function = AcqFunctionEI$new(surrogate = SURR_KM_DETERM),
    acq_optimizer = ACQ_OPT_DEF
  )
  expect_class(optim, "OptimizerMbo")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10))

  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)

  res = optim$optimize(instance)
  expect_equal(res, instance$result)

  opdf = instance$archive$data
  expect_data_table(opdf, any.missing = TRUE, nrows = 10)
  expect_data_table(tail(opdf, - nrow(design)), any.missing = FALSE, nrows = 10 - nrow(design))
  expect_equal(instance$result$y, 0, tolerance = 0.1)

  optim$optimize(instance)
})


test_that("OptimizerMbo works with different settings", {

  # define combinations
  loop_functions = list(
    soo = list(fun = bayesopt_soo),
    mpcl = list(fun = bayesopt_mpcl, args = list(liar = mean, q = 2))
  )
  acq_functions = list(
    aei = AcqFunctionAEI$new(SURR_KM_NOISY),
    cb = AcqFunctionCB$new(SURR_KM_DETERM),
    ei = AcqFunctionEI$new(SURR_KM_DETERM)
  )
  acq_optimizers = list(
    rs = ACQ_OPT_DEF
  )

  combinations = cross_join(list(
    loop_function = loop_functions,
    acq_function = acq_functions,
    acq_optimizer = acq_optimizers
  ), sorted = FALSE)

  for (i in seq_row(combinations)) {
    mbo_fun = combinations[i,"loop_function"][[1]][[1]]$fun
    args = combinations[i,"loop_function"][[1]][[1]]$args
    acq_fun = combinations[i,"acq_function"][[1]][[1]]
    acq_opt = combinations[i,"acq_optimizer"][[1]][[1]]
    optim = OptimizerMbo$new(
      loop_function = mbo_fun,
      acq_function = acq_fun,
      acq_optimizer = acq_opt,
      args = args
    )

    instance = MAKE_INST(terminator = trm("evals", n_evals = 12))

    design = MAKE_DESIGN(instance, 6)
    instance$eval_batch(design)

    optim$optimize(instance)

    opdf = instance$archive$data
    expect_data_table(tail(opdf, -6), any.missing = FALSE, nrows = 12 - 6)
    expect_equal(instance$result$y, 0, tolerance = 0.6)
  }
})

test_that("OptimizerMbo works for noisy problems", {

  obfun = OBJ_2D_NOISY

  optim = OptimizerMbo$new(
    loop_function = bayesopt_soo,
    acq_function = AcqFunctionAEI$new(surrogate = SURR_KM_NOISY),
    acq_optimizer = ACQ_OPT_DEF,
    result_function = result_by_surrogate_design,
  )

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = 20),
    search_space = PS_2D
  )

  design = MAKE_DESIGN(instance, 12)
  instance$eval_batch(design)

  optim$optimize(instance)

  opdf = instance$archive$data
  expect_data_table(tail(opdf, -12), any.missing = FALSE, nrows = 20 - 12)
  #FIXME: Can we test that the surrogate actually influneces the choice?
  #expect_true(instance$result$y > min(opdf$y)) # we have not chosen the overoptimistic noisy y
  #expect_equal(instance$result$y, 0, tolerance = 0.1)
})

