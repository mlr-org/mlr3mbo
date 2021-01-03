test_that("bayesopt_mpcl", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  acq_function = AcqFunctionEI$new(surrogate = SURR_KM_DETERM)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_mpcl(instance, acq_function = acq_function, acq_optimizer = acq_optimizer, liar = mean, q = 2L)
  expect_true(nrow(instance$archive$data) == 6L)
})
