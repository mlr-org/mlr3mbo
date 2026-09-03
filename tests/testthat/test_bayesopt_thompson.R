SUBSPACES_1D_BRANCH_THOMPSON = partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = "b"))

MAKE_ACQ_OPTIMIZER = function() {
  AcqOptimizer$new(opt("random_search", batch_size = 5L), terminator = trm("evals", n_evals = 5L))
}

test_that("bayesopt_thompson class", {
  expect_loop_function(bayesopt_thompson)
})

test_that("bayesopt_thompson keeps every proposal in the sampled subspace", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 10L))
  bayesopt_thompson(
    instance,
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER(),
    param = "branch",
    init_design_size = 2L
  )

  data = instance$archive$data
  expect_data_table(data, nrows = 10L)
  # every point belongs to the subspace of its branch and only holds the parameters of that subspace
  expect_equal(data$.subspace, data$branch)
  expect_true(all(is.na(data[data$branch == "a", ]$xb)))
  expect_true(all(is.na(data[data$branch == "a", ]$fb)))
  expect_true(all(is.na(data[data$branch == "b", ]$xa)))
  expect_true(all(!is.na(data$shared)))
  # the initial design covers every subspace
  expect_set_equal(data$.subspace[1:4], c("a", "b"))
})

test_that("bayesopt_thompson restores the surrogate after termination", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 6L))
  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  bayesopt_thompson(
    instance,
    surrogate = surrogate,
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER(),
    param = "branch",
    init_design_size = 2L
  )

  expect_equal(surrogate$archive, instance$archive)
  expect_set_equal(surrogate$cols_x, instance$archive$cols_x)
})

test_that("bayesopt_thompson generates one initial design per subspace", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 40L))
  bayesopt_thompson(
    instance,
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER(),
    param = "branch"
  )

  # `4 * d` per subspace with `d` being the dimensionality of the respective subspace
  sizes = map_int(SUBSPACES_1D_BRANCH_THOMPSON, function(subspace) 4L * subspace$length)
  design = instance$archive$data$.subspace[seq_len(sum(sizes))]
  expect_equal(map_int(names(sizes), function(subspace_id) sum(design == subspace_id)), unname(sizes))
})

test_that("bayesopt_thompson respects a user-supplied initial design", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 8L))
  design = generate_design_random(SUBSPACES_1D_BRANCH_THOMPSON$a, n = 5L)$data
  instance$eval_batch(pad_xdt(design, cols_x = instance$archive$cols_x, na_values = na_values(PS_1D_BRANCH)))
  bayesopt_thompson(
    instance,
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER(),
    param = "branch",
    init_design_size = 3L
  ) # ignored

  expect_data_table(instance$archive$data, nrows = 8L)
  expect_equal(instance$archive$data$.subspace[1:5], rep("a", 5L))
})

test_that("bayesopt_thompson groups levels", {
  search_space = ps(
    branch = p_fct(c("a", "b", "c")),
    xa = p_dbl(-1, 1, depends = branch == "a"),
    xb = p_dbl(-1, 1, depends = branch == "b"),
    xc = p_dbl(-1, 1, depends = branch == "c")
  )
  fun = function(xs) list(y = switch(xs$branch, a = xs$xa, b = xs$xb, c = xs$xc)^2)
  objective = bbotk::ObjectiveRFun$new(fun = fun, domain = search_space, codomain = FUN_1D_CODOMAIN)
  instance = MAKE_INST(objective = objective, search_space = search_space, terminator = trm("evals", n_evals = 10L))

  bayesopt_thompson(
    instance,
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER(),
    param = "branch",
    groups = list(ab = c("a", "b"), c = "c"),
    init_design_size = 2L
  )

  data = instance$archive$data
  expect_subset(data$.subspace, c("ab", "c"))
  expect_equal(data$.subspace, ifelse(data$branch == "c", "c", "ab"))
})

test_that("bayesopt_thompson random interleaving stays within the sampled subspace", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 10L))
  bayesopt_thompson(
    instance,
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER(),
    param = "branch",
    init_design_size = 2L,
    random_interleave_iter = 2L
  )

  data = instance$archive$data
  expect_data_table(data, nrows = 10L)
  expect_equal(data$.subspace, data$branch)
  # the interleaved points are sampled at random and therefore carry no acquisition function value
  expect_equal(sum(is.na(data$acq_ei[5:10])), 3L)
})

test_that("bayesopt_thompson asserts its arguments", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 6L))
  args = list(
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER()
  )

  expect_error(invoke(bayesopt_thompson, instance, param = "xa", .args = args), "must be a 'ParamFct'")
  expect_error(invoke(bayesopt_thompson, instance, param = "missing", .args = args), "Must be element of set")
  expect_error(
    invoke(bayesopt_thompson, instance, param = "branch", groups = list(a = "a"), .args = args),
    "levels of the groups"
  )
  expect_error(
    invoke(bayesopt_thompson, instance, param = "branch", init_design_size = 0L, .args = args),
    "init_design_size"
  )
  expect_error(
    invoke(bayesopt_thompson, instance, param = "branch", top_quantile = 1.5, .args = args),
    "top_quantile"
  )
})

test_that("bayesopt_thompson works with OptimizerMbo", {
  instance = MAKE_INST(objective = OBJ_1D_BRANCH, search_space = PS_1D_BRANCH, terminator = trm("evals", n_evals = 8L))
  optimizer = opt(
    "mbo",
    loop_function = bayesopt_thompson,
    args = list(param = "branch", init_design_size = 2L),
    surrogate = SurrogateLearner$new(REGR_FEATURELESS),
    acq_function = AcqFunctionEI$new(),
    acq_optimizer = MAKE_ACQ_OPTIMIZER()
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)
  expect_data_table(instance$archive$data, nrows = 8L)
  expect_equal(instance$archive$data$.subspace, instance$archive$data$branch)
})

test_that("bayesopt_thompson is registered", {
  expect_equal(mlr_loop_functions$get("bayesopt_thompson"), bayesopt_thompson)
})

test_that("assign_subspaces assigns every point to exactly one subspace", {
  data = generate_design_random(PS_1D_BRANCH, n = 20L)$data
  assignment = assign_subspaces(SUBSPACES_1D_BRANCH_THOMPSON, data)
  expect_character(assignment, len = 20L, any.missing = FALSE)
  expect_equal(assignment, data$branch)
})

test_that("top_quantile_hits marks the best fraction of the outcomes", {
  expect_equal(top_quantile_hits(seq(20, 1), top_quantile = 0.1), c(rep(FALSE, 18L), TRUE, TRUE))
  # the single best outcome is always a hit
  expect_equal(top_quantile_hits(c(3, 1, 2), top_quantile = 0.1), c(FALSE, TRUE, FALSE))
  # ties at the boundary are all hits
  expect_equal(top_quantile_hits(c(1, 2, 1, 3), top_quantile = 0.1), c(TRUE, FALSE, TRUE, FALSE))
  # failed evaluations are never hits and do not count towards the fraction
  expect_equal(top_quantile_hits(c(5, NA, 3), top_quantile = 0.5), c(FALSE, FALSE, TRUE))
  expect_equal(top_quantile_hits(c(NA_real_, NA_real_), top_quantile = 0.1), c(FALSE, FALSE))
  expect_equal(top_quantile_hits(numeric(), top_quantile = 0.1), logical())
})

test_that("thompson_sample_subspace prefers the subspace with the top-quantile hits", {
  # all top-decile outcomes belong to 'a'
  subspace = rep(c("a", "b"), each = 50L)
  y = c(seq(50, 1), rep(100, 50L))
  sampled = replicate(
    100L,
    thompson_sample_subspace(c("a", "b"), subspace, y, top_quantile = 0.1, alpha = 1, beta = 1)
  )
  expect_gt(sum(sampled == "a"), 90L)
})

test_that("thompson_sample_subspace explores an unevaluated subspace", {
  subspace = rep("a", 10L)
  y = seq(10, 1)
  sampled = replicate(
    100L,
    thompson_sample_subspace(c("a", "b"), subspace, y, top_quantile = 0.1, alpha = 1, beta = 1)
  )
  expect_set_equal(unique(sampled), c("a", "b"))
})

test_that("thompson_sample_subspace counts failed evaluations as failures", {
  subspace = rep(c("a", "b"), each = 2L)
  y = c(2, 1, NA, NA)
  sampled = replicate(
    100L,
    thompson_sample_subspace(c("a", "b"), subspace, y, top_quantile = 0.1, alpha = 1, beta = 1)
  )
  expect_gt(sum(sampled == "a"), 50L)
})
