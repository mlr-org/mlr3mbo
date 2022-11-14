test_that("bayesopt_parego class", {
  expect_loop_function(bayesopt_parego)
})

test_that("default bayesopt_parego", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_parego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true("y_scal" %in% colnames(instance$archive$data))
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
})

