test_that("InputTrafoUnitcube works", {
  instance = MAKE_INST_1D()
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  it = InputTrafoUnitcube$new()
  it$cols_x = instance$archive$cols_x
  it$search_space = instance$archive$search_space

  it$update(instance$archive$data)
  expect_list(it$state, len = 0L)
  data = instance$archive$data
  orig_data = copy(data)
  transformed_data = it$transform(data)
  expect_true(all(apply(transformed_data[, instance$archive$cols_x, with = FALSE], MARGIN = 2L, FUN = min) >= 0))
  expect_true(all(apply(transformed_data[, instance$archive$cols_x, with = FALSE], MARGIN = 2L, FUN = max) <= 1))
  expect_false(address(data) == address(transformed_data))
  expect_equal(data, orig_data)
})

test_that("InputTrafoUnitcube works with SurrogateLearner", {
  instance = MAKE_INST_1D()
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  surrogate$archive = instance$archive
  it = InputTrafoUnitcube$new()
  surrogate$input_trafo = it

  surrogate$update()
  expect_list(it$state, len = 0L)
  surrogate$predict(instance$archive$data)
})

test_that("InputTrafoUnitcube works with SurrogateLearnerCollection", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  surrogate$archive = instance$archive
  it = InputTrafoUnitcube$new()
  surrogate$input_trafo = it

  surrogate$update()
  expect_list(it$state, len = 0L)
  surrogate$predict(instance$archive$data)
})

test_that("InputTrafoUnitcube works with OptimizerMbo and bayesopt_ego", {
  instance = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  it = InputTrafoUnitcube$new()
  surrogate$input_trafo = it
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_ego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, nrows = 1L)
})

test_that("InputTrafoUnitcube works with OptimizerMbo and bayesopt_parego", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  it = InputTrafoUnitcube$new()
  surrogate$input_trafo = it
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_parego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, min.rows = 1L)
})

test_that("InputTrafoUnitcube works with OptimizerMbo and bayesopt_smsego", {
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  it = InputTrafoUnitcube$new()
  surrogate$input_trafo = it
  acq_function = AcqFunctionSmsEgo$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_smsego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, min.rows = 1L)
})

