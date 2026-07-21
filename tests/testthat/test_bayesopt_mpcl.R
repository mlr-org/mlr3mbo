test_that("bayesopt_mpcl class", {
  expect_loop_function(bayesopt_mpcl)
})

test_that("default bayesopt_mpcl", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_mpcl(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(sum(is.na(instance$archive$data$acq_ei)) == 4L)
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
  expect_true(!is.na(instance$archive$data$acq_ei[6L]))
  expect_true(all(instance$archive$data$batch_nr[c(5L, 6L)] == c(2L, 2L)))
})

test_that("bayesopt_mpcl random interleave triggers per proposal", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 8L))
  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  bayesopt_mpcl(
    instance,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    init_design_size = 4L,
    q = 2L,
    random_interleave_iter = 2L
  )

  expect_identical(is.na(instance$archive$data$acq_ei), c(rep(TRUE, 4L), FALSE, TRUE, FALSE, TRUE))
})
