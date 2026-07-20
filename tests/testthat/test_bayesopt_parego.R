test_that("bayesopt_parego class", {
  expect_loop_function(bayesopt_parego)
})

test_that("default bayesopt_parego", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))
  bayesopt_parego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_true(sum(is.na(instance$archive$data$acq_ei)) == 4L)
  expect_true("y_scal" %in% colnames(instance$archive$data))
  expect_true(!is.na(instance$archive$data$acq_ei[5L]))
  expect_true(is.na(instance$archive$data$y_scal[5L]))
})

test_that("bayesopt_parego aligns the multiplier with the target columns", {
  fun = function(xs) list(y1 = as.numeric(xs)^2, time = 1, y2 = -sqrt(abs(as.numeric(xs))))
  objective = bbotk::ObjectiveRFun$new(
    fun = fun,
    domain = PS_1D,
    codomain = ps(y1 = p_dbl(tags = "minimize"), time = p_dbl(tags = "time"), y2 = p_dbl(tags = "maximize")),
    properties = "multi-crit"
  )
  instance = OptimInstanceBatchMultiCrit$new(objective = objective, terminator = trm("evals", n_evals = 6L))
  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  expect_no_warning(bayesopt_parego(
    instance,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    init_design_size = 4L
  ))
  expect_data_table(instance$archive$data, nrows = 6L)
})

test_that("bayesopt_parego works with a constant objective", {
  fun = function(xs) list(y1 = as.numeric(xs)^2, y2 = 1)
  objective = bbotk::ObjectiveRFun$new(
    fun = fun,
    domain = PS_1D,
    codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize")),
    properties = "multi-crit"
  )
  instance = OptimInstanceBatchMultiCrit$new(objective = objective, terminator = trm("evals", n_evals = 6L))
  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  bayesopt_parego(
    instance,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    init_design_size = 4L
  )

  expect_true(all(is.finite(head(instance$archive$data$y_scal, 5L))))
  expect_true(all(!is.na(tail(instance$archive$data$acq_ei, 2L))))
})
