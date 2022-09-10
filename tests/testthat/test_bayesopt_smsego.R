test_that("bayesopt_smsego class", {
  expect_loop_function(bayesopt_smsego)
})

test_that("default bayesopt_sms", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  bayesopt_smsego(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})

