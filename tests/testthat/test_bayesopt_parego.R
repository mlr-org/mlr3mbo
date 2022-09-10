test_that("bayesopt_parego class", {
  expect_loop_function(bayesopt_parego)
})

test_that("default bayesopt_parego", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  bayesopt_parego(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true("y_scal" %in% colnames(instance$archive$data))
})

