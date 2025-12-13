test_that("ResultAssignerArchive works", {
  ras = ResultAssignerArchive$new()

  instance = MAKE_INST_1D()
  design = generate_design_random(instance$search_space, n = 4L)$data
  instance$eval_batch(design)

  expect_null(instance$result)
  ras$assign_result(instance)
  expect_data_table(instance$result, nrows = 1L)
  expect_equal(instance$result[[instance$archive$cols_x]], instance$archive$best()[[instance$archive$cols_x]])
  expect_equal(instance$result[[instance$archive$cols_y]], instance$archive$best()[[instance$archive$cols_y]])
})

test_that("ResultAssignerArchive works with OptimizerMbo and bayesopt_ego", {
  skip_if_missing_regr_km()
  result_assigner = ResultAssignerArchive$new()

  instance = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_ego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, result_assigner = result_assigner)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, nrows = 1L)
})

test_that("ResultAssignerArchive works with OptimizerMbo and bayesopt_parego", {
  skip_if_missing_regr_km()
  result_assigner = ResultAssignerArchive$new()

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_parego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, result_assigner = result_assigner)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, min.rows = 1L)
})

test_that("ResultAssignerArchive works with OptimizerMbo and bayesopt_smsego", {
  skip_if_missing_regr_km()
  result_assigner = ResultAssignerArchive$new()

  instance = MAKE_INST(OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  surrogate = SurrogateLearnerCollection$new(list(REGR_KM_DETERM, REGR_KM_DETERM$clone(deep = TRUE)))
  acq_function = AcqFunctionSmsEgo$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  optimizer = opt("mbo", loop_function = bayesopt_smsego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, result_assigner = result_assigner)
  optimizer$optimize(instance)
  expect_true(nrow(instance$archive$data) == 5L)
  expect_data_table(instance$result, min.rows = 1L)
})

test_that("ResultAssignerArchive passes internal tuned values", {
  skip_if_missing_regr_km()
  result_assigner = ResultAssignerArchive$new()

  learner = lrn("classif.debug",
    validate = 0.2,
    early_stopping = TRUE,
    x = to_tune(0.2, 0.3),
    iter = to_tune(upper = 1000L, internal = TRUE, aggr = function(x) 99L))

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20L),
    store_benchmark_result = TRUE
  )
  surrogate = SurrogateLearner$new(REGR_KM_DETERM)
  acq_function = AcqFunctionEI$new()
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 2L), terminator = trm("evals", n_evals = 2L))

  tuner = tnr("mbo", result_assigner = result_assigner)
  expect_data_table(tuner$optimize(instance), nrows = 1L)
  expect_list(instance$archive$data$internal_tuned_values, len = 20L, types = "list")
  expect_equal(instance$archive$data$internal_tuned_values[[1L]]$iter, 99L)
  expect_false(instance$result_learner_param_vals$early_stopping)
  expect_equal(instance$result_learner_param_vals$iter, 99L)
})
