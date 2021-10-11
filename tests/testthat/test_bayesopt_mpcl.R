test_that("default bayesopt_mpcl", {
  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 5L))
  bayesopt_mpcl(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})
