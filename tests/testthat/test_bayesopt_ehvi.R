test_that("bayesopt_ehvi with ehvi", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  bayesopt_ehvi(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})

