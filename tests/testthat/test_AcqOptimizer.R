test_that("AcqOptimizer API works", {
  ### EI, random search
  instance = OptimInstanceSingleCrit$new(OBJ_1D, terminator = trm("evals", n_evals = 1L))
  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)
  acqfun = AcqFunctionEI$new(SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L), acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)

  ### upgrading error class works
  acqopt = AcqOptimizer$new(OptimizerError$new(), trm("evals", n_evals = 2L), acq_function = acqfun)
  expect_error(acqopt$optimize(), class = c("mbo_error", "optimize_error"))
})

test_that("AcqOptimizer param_set", {
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L))
  expect_r6(acqopt$param_set, "ParamSet")
  expect_true(all(c("fix_distance", "dist_threshold") %in% acqopt$param_set$ids()))
  expect_r6(acqopt$param_set$params$fix_distance, "ParamLgl")
  expect_r6(acqopt$param_set$params$dist_threshold, "ParamDbl")
  expect_error({acqopt$param_set = list()}, regexp = "param_set is read-only.")
})

test_that("AcqOptimizer fix_xdt_distance", {
  domain = ParamSet$new(list(
    ParamDbl$new("x1", -5, 5),
    ParamFct$new("x2", levels = c("a", "b", "c")),
    ParamInt$new("x3", 1L, 2L),
    ParamLgl$new("x4"))
  )

  dist_threshold = 0

  ### single point proposal to multiple previous points
  previous_xdt = data.table(x1 = c(0.1, 0.2, 0.3, 0.4), x2 = c("a", "b", "c", "a"), x3 = c(1L, 2L, 1L, 2L), x4 = c(TRUE, TRUE, FALSE, FALSE))
  xdt_ok = data.table(x1 = 0.5, x2 = "b", x3 = 1L, x4 = TRUE)
  expect_equal(address(fix_xdt_distance(xdt_ok, previous_xdt = previous_xdt, search_space = domain, dist_threshold = dist_threshold)), address(xdt_ok))  # no change at all, not even a copy
  xdt_redundant = previous_xdt[1L, ]
  xdt_fixed = fix_xdt_distance(xdt_redundant, previous_xdt = previous_xdt, search_space = domain, dist_threshold = dist_threshold)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 1L)
  expect_true(check_gower_dist(get_gower_dist(xdt_fixed, previous_xdt), dist_threshold))

  ### single point proposal to single previous point
  expect_equal(address(fix_xdt_distance(xdt_ok, previous_xdt = previous_xdt[4L, ], search_space = domain, dist_threshold = dist_threshold)), address(xdt_ok))  # no change at all, not even a copy
  xdt_redundant = previous_xdt[4L, ]
  xdt_fixed = fix_xdt_distance(xdt_redundant, previous_xdt = previous_xdt[4L, ], search_space = domain, dist_threshold = dist_threshold)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 1L)
  expect_true(check_gower_dist(get_gower_dist(xdt_fixed, previous_xdt[4L, ]), dist_threshold))

  # multiple point proposal to multiple previous points
  xdt_ok = rbind(xdt_ok, data.table(x1 = 0.6, x2 = "c", x3 = 2L, x4 = FALSE))
  expect_equal(address(fix_xdt_distance(xdt_ok, previous_xdt = previous_xdt, search_space = domain, dist_threshold = dist_threshold)), address(xdt_ok))  # no change at all, not even a copy
  xdt_redundant = rbind(xdt_ok, previous_xdt[3:4, ])
  xdt_fixed = fix_xdt_distance(xdt_redundant, previous_xdt = previous_xdt, search_space = domain, dist_threshold = dist_threshold)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 4L)
  expect_true(identical(xdt_fixed[1:2, ], xdt_redundant[1:2, ]))
  expect_true(all(check_gower_dist(get_gower_dist(xdt_fixed, previous_xdt), dist_threshold)))

  # multiple point proposal to single previous point
  expect_equal(address(fix_xdt_distance(xdt_ok, previous_xdt = previous_xdt[4L, ], search_space = domain, dist_threshold = dist_threshold)), address(xdt_ok))  # no change at all, not even a copy
  xdt_fixed = fix_xdt_distance(xdt_redundant, previous_xdt = previous_xdt[4L, ], search_space = domain, dist_threshold = dist_threshold)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 4L)
  expect_true(identical(xdt_fixed[1:3, ], xdt_redundant[1:3, ]))
  expect_true(all(check_gower_dist(get_gower_dist(xdt_fixed, previous_xdt[4L, ]), dist_threshold)))

  # multiple point proposal to multiple previous points, threshold higher to allow for second loop testing
  withr::local_seed(1)
  dist_threshold = 1
  xdt_fixed = fix_xdt_distance(xdt_redundant, previous_xdt = previous_xdt, search_space = domain, dist_threshold = dist_threshold)
  check_gower_dist(get_gower_dist(xdt_fixed, previous_xdt), dist_threshold = dist_threshold)
})

test_that("AcqOptimizer trafo", {
  domain = ps(x = p_dbl(lower = 10, upper = 20, trafo = function(x) x - 15))
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2),
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )
  instance = MAKE_INST(objective = objective, search_space = domain, terminator = trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)
  acqfun = AcqFunctionEI$new(SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L), acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()
  res = acqopt$optimize()
  expect_equal(res$x, res$x_domain[[1L]][[1L]])
})

