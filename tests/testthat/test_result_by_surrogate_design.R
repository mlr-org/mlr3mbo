test_that("result_by_surrogate_design works", {
  instance = MAKE_INST(OBJ_1D_2_NOISY, PS_1D, trm("evals", n_evals = 10))
  optimizer = OptimizerMbo$new()
  optimizer$optimize(instance)
  out = result_by_surrogate_design(instance, optimizer)
  expect_r6(out, classes = "OptimInstance")
  expect_data_table(instance$result, min.rows = 1L)
})
