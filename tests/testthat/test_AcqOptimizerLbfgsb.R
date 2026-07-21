test_that("AcqOptimizerLbfgsb works", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  # L-BFGS-B starts at and can return the incumbent here, so disable skip_already_evaluated for this smoke test
  acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "none", skip_already_evaluated = FALSE)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
  expect_names(names(acqopt$state), must.include = "iteration_1")
  expect_class(acqopt$state$iteration_1$model, "nloptr")
  expect_true(acqopt$state$iteration_1$model$iterations <= 200L)
})

test_that("AcqOptimizerLbfgsb rejects an already evaluated candidate when skip_already_evaluated is TRUE", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  instance$eval_batch(generate_design_grid(instance$search_space, resolution = 4L)$data)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "none")
  acqfun$surrogate$update()
  acqfun$update()

  # the incumbent is re-proposed, which is rejected by default and accepted with skip_already_evaluated = FALSE
  expect_error(acqopt$optimize(), class = "Mlr3ErrorMboAcqOptimizer")
  acqopt$param_set$set_values(skip_already_evaluated = FALSE)
  expect_data_table(acqopt$optimize(), nrows = 1L)
})

test_that("AcqOptimizerLbfgsb does not expose the dead minf_max parameter", {
  expect_false("minf_max" %in% AcqOptimizerLbfgsb$new()$param_set$ids())
})

test_that("AcqOptimizerLbfgsb maxeval accepts -1L and -1 to deactivate", {
  acqopt = AcqOptimizerLbfgsb$new()
  expect_error(acqopt$param_set$set_values(maxeval = -1L), NA)
  expect_error(acqopt$param_set$set_values(maxeval = -1), NA)
})

test_that("AcqOptimizerLbfgsb raises an acq optimizer error when no valid solution is found", {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  instance$eval_batch(generate_design_grid(instance$search_space, resolution = 4L)$data)
  surrogate = srlrn(REGR_FEATURELESS, archive = instance$archive)

  AcqFunctionNaN = R6::R6Class(
    "AcqFunctionNaN",
    inherit = AcqFunction,
    public = list(initialize = function(surrogate = NULL) {
      super$initialize(
        "acq_nan",
        surrogate = surrogate,
        requires_predict_type_se = FALSE,
        surrogate_class = "SurrogateLearner",
        direction = "minimize"
      )
    }),
    private = list(.fun = function(xdt) data.table(acq_nan = rep(NaN, nrow(xdt))))
  )
  acqfun = AcqFunctionNaN$new(surrogate = surrogate)
  acqfun$surrogate$update()
  acqfun$update()
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 50L, restart_strategy = "none")
  expect_error(acqopt$optimize(), class = "Mlr3ErrorMboAcqOptimizer")
})

test_that("AcqOptimizerLbfgsb resets state between optimize() calls", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  # L-BFGS-B starts at and can return the incumbent here, so disable skip_already_evaluated for this test
  acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "none", skip_already_evaluated = FALSE)
  acqfun$surrogate$update()
  acqfun$update()

  acqopt$optimize()
  acqopt$optimize()
  expect_length(acqopt$state, 1L)

  acqopt$reset()
  expect_null(acqopt$state)
})

test_that("AcqOptimizerLbfgsb works with 2D", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state)
  expect_names(names(acqopt$state), must.include = "iteration_1")
  expect_class(acqopt$state$iteration_1$model, "nloptr")
  expect_true(acqopt$state$iteration_1$model$iterations <= 200L)
})

test_that("AcqOptimizerLbfgsb works with instance", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 10L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 10L)

  optimizer = opt("mbo", acq_optimizer = acqopt, acq_function = acqfun, surrogate = surrogate)
  expect_data_table(optimizer$optimize(instance), nrows = 1L)
})

test_that("AcqOptimizerLbfgsb works when the incumbent lies on a search space bound", {
  skip_if_missing_regr_km()
  # linear objective, minimized -> the incumbent lies exactly on the lower bound
  obj = bbotk::ObjectiveRFun$new(
    fun = function(xs) list(y = as.numeric(xs$x)),
    domain = PS_1D,
    codomain = ps(y = p_dbl(tags = "minimize")),
    properties = "single-crit"
  )
  instance = oi(obj, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = 200L, restart_strategy = "none")
  acqfun$surrogate$update()
  acqfun$update()

  # the incumbent is at the lower bound, which used to abort nloptr with "at least one element in x0 < lb"
  expect_equal(acqfun$archive$best()[["x"]], unname(instance$search_space$lower))
  res = acqopt$optimize()
  expect_data_table(res, nrows = 1L)
  expect_number(res[["x"]], lower = instance$search_space$lower, upper = instance$search_space$upper)
})

test_that("AcqOptimizerLbfgsb works with random restart", {
  skip_if_missing_regr_km()
  instance = oi(OBJ_2D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)

  surrogate = srlrn(REGR_KM_DETERM, archive = instance$archive)
  acqfun = acqf("ei", surrogate = surrogate)
  acqopt = AcqOptimizerLbfgsb$new(acq_function = acqfun)
  acqopt$param_set$set_values(maxeval = -1, ftol_rel = 1e-6, restart_strategy = "random", max_restarts = 3L)
  acqfun$surrogate$update()
  acqfun$update()

  expect_data_table(acqopt$optimize(), nrows = 1L)
  expect_list(acqopt$state, min.len = 4L)
  expect_names(names(acqopt$state), must.include = c("iteration_1", "iteration_2", "iteration_3", "iteration_4"))
  walk(acqopt$state, function(x) expect_class(x$model, "nloptr"))
  expect_true(all(sapply(acqopt$state, function(x) x$model$iterations) <= 50L))
})
