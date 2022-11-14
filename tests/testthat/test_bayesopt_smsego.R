test_that("bayesopt_smsego class", {
  expect_loop_function(bayesopt_smsego)
})

test_that("default bayesopt_sms", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  acq_function = AcqFunctionSmsEgo$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_smsego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(!is.na(instance$archive$data$acq_sms_ego[5L]))
})

