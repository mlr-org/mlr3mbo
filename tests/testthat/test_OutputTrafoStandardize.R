test_that("OutputTrafoStandardize works", {
  instance = MAKE_INST_1D()
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  ot = OutputTrafoStandardize$new()
  ot$cols_y = instance$archive$cols_y
  ot$max_to_min = instance$objective_multiplicator

  ot$update(instance$archive$data)
  expect_list(ot$state, len = length(ot$cols_y))
  data = instance$archive$data
  orig_data = copy(data)
  transformed_data = ot$transform(data)
  expect_data_table(transformed_data, nrows = nrow(data), ncols = ncol(data))
  expect_false(address(data) == address(transformed_data))
  inverse_transformed_data = ot$inverse_transform(transformed_data)
  expect_equal(data, inverse_transformed_data)
  expect_false(address(data) == address(inverse_transformed_data))

  pred = data.table(mean = mean(transformed_data[[ot$cols_y]]), se = sd(transformed_data[[ot$cols_y]]))
  inverse_transformed_pred = ot$inverse_transform_posterior(pred)
  expect_false(address(pred) == address(inverse_transformed_pred))

  expect_equal(data, orig_data)
})

test_that("OutputTrafoStandardize works with SurrogateLearner", {
  skip_if_missing_regr_km()
  instance = MAKE_INST_1D()
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  surrogate$archive = instance$archive
  ot = OutputTrafoStandardize$new()
  surrogate$output_trafo = ot

  surrogate$update()
  expect_list(surrogate$output_trafo$state, len = length(surrogate$cols_y))

  expect_false(surrogate$output_trafo_must_be_considered)
  inverse_transformed_pred = surrogate$predict(instance$archive$data)
  surrogate$output_trafo$invert_posterior = FALSE

  expect_true(surrogate$output_trafo_must_be_considered)
  pred = surrogate$predict(instance$archive$data)
})

test_that("OutputTrafoStandardize works with SurrogateLearnerCollection", {
  skip_if_missing_regr_km()
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  surrogate$archive = instance$archive
  ot = OutputTrafoStandardize$new()
  surrogate$output_trafo = ot

  surrogate$update()
  expect_list(surrogate$output_trafo$state, len = length(surrogate$cols_y))

  expect_false(surrogate$output_trafo_must_be_considered)
  inverse_transformed_pred = surrogate$predict(instance$archive$data)
  surrogate$output_trafo$invert_posterior = FALSE

  expect_true(surrogate$output_trafo_must_be_considered)
  pred = surrogate$predict(instance$archive$data)
})

test_that("OutputTrafoStandardize works with OptimizerMbo and bayesopt_ego", {
  skip_if_missing_regr_km()
  instance = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  ot = OutputTrafoStandardize$new()
  surrogate$output_trafo = ot
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_ego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, nrows = 1L)
})

test_that("OutputTrafoStandardize works with OptimizerMbo and bayesopt_parego", {
  skip_if_missing_regr_km()
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  ot = OutputTrafoStandardize$new()
  surrogate$output_trafo = ot
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_parego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, min.rows = 1L)
  expect_true(ot$cols_y == "y_scal")
})

test_that("OutputTrafoStandardize works with OptimizerMbo and bayesopt_smsego", {
  skip_if_missing_regr_km()
  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  ot = OutputTrafoStandardize$new()
  surrogate$output_trafo = ot
  acq_function = AcqFunctionSmsEgo$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_smsego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, min.rows = 1L)
})

