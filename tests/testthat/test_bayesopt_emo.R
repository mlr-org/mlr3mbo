test_that("bayesopt_emo class", {
  expect_loop_function(bayesopt_emo)
})

test_that("default bayesopt_emo", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  acq_function = AcqFunctionEHVI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_emo(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(sum(is.na(instance$archive$data$acq_ehvi)) == 4L)
  expect_true(!is.na(instance$archive$data$acq_ehvi[5L]))

  skip_if_not_installed("emoa")
  skip_if_not_installed("fastGHQuad")
  instance$archive$clear()
  acq_function = AcqFunctionEHVIGH$new()
  bayesopt_emo(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(sum(is.na(instance$archive$data$acq_ehvigh)) == 4L)
  expect_true(!is.na(instance$archive$data$acq_ehvigh[5L]))
})

