test_that("bayesopt_mego with ehvi", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  bayesopt_mego(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})

