test_that("default bayesopt_soo", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  bayesopt_soo(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})
