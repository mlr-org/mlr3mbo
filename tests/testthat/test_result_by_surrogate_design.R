test_that("result_by_surrogate_design works - single-crit", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST(OBJ_1D_NOISY, PS_1D, trm("evals", n_evals = 10))
  optimizer = OptimizerMbo$new(loop_function = default_loopfun(instance),
                               surrogate = default_surrogate(instance),
                               acq_function = default_acqfun(instance),
                               acq_optimizer = default_acqopt(default_acqfun(instance)))
  optimizer$surrogate$archive = instance$archive
  optimizer$acq_function$surrogate = optimizer$surrogate
  opt("random_search")$optimize(instance)  # optimize with rs to not use the optimizers own result_function
  orig = copy(instance$archive$data)
  out = result_by_surrogate_design(instance, optimizer)
  expect_r6(out, classes = "OptimInstance")
  expect_data_table(instance$result, min.rows = 1L)
  # ys are still the same
  expect_equal(instance$result[, instance$archive$cols_y, with = FALSE],
               orig[instance$result, on = instance$archive$cols_x][, instance$archive$cols_y, with = FALSE])
})

test_that("result_by_surrogate_design works - multi-crit", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST(OBJ_1D_2_NOISY, PS_1D, trm("evals", n_evals = 10))
  optimizer = OptimizerMbo$new(loop_function = default_loopfun(instance),
                               surrogate = default_surrogate(instance),
                               acq_function = default_acqfun(instance),
                               acq_optimizer = default_acqopt(default_acqfun(instance)))
  optimizer$surrogate$archive = instance$archive
  optimizer$acq_function$surrogate = optimizer$surrogate
  opt("random_search")$optimize(instance)  # optimize with rs to not use the optimizers own result_function
  orig = copy(instance$archive$data)
  out = result_by_surrogate_design(instance, optimizer)
  expect_r6(out, classes = "OptimInstance")
  expect_data_table(instance$result, min.rows = 1L)
  # ys are still the same
  expect_equal(instance$result[, instance$archive$cols_y, with = FALSE],
               orig[instance$result, on = instance$archive$cols_x][, instance$archive$cols_y, with = FALSE])
})

