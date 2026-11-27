skip_if_not_installed("rush")
skip_if_no_redis()

# branching 1D function with three algorithms, so that a compute profile can be shared by two of them
PS_1D_BRANCH3 = ps(
  branch = p_fct(c("a", "b", "c")),
  xa = p_dbl(-1, 1, depends = branch == "a"),
  xb = p_dbl(-1, 1, depends = branch == "b"),
  xc = p_dbl(0, 1, depends = branch == "c"),
  shared = p_dbl(0, 1)
)
FUN_1D_BRANCH3 = function(xs) {
  list(y = switch(xs$branch, a = xs$xa^2, b = abs(xs$xb), c = xs$xc + 1) + xs$shared)
}
OBJ_1D_BRANCH3 = bbotk::ObjectiveRFun$new(
  fun = FUN_1D_BRANCH3,
  domain = PS_1D_BRANCH3,
  codomain = FUN_1D_CODOMAIN,
  properties = "single-crit"
)

make_instance = function(rush, n_evals) {
  oi_async(
    objective = OBJ_1D_BRANCH3,
    search_space = PS_1D_BRANCH3,
    terminator = trm("evals", n_evals = n_evals),
    rush = rush
  )
}

test_that("OptimizerADBOThompson samples among all subspaces by default", {
  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  instance = make_instance(rush, n_evals = 30L)
  optimizer = opt("adbo_thompson", branch = "branch", design_size = 2L, n_workers = 2L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  data = instance$archive$data
  expect_data_table(data, min.rows = 30L)
  expect_names(names(data), must.include = c("acq_cb", "acq_lambda", "acq_lambda_0"))
  finished = data[data$state == "finished", ]
  # every point only holds the parameters of its subspace
  expect_true(all(is.na(finished[finished$branch != "a", ]$xa)))
  expect_true(all(is.na(finished[finished$branch != "b", ]$xb)))
  expect_true(all(is.na(finished[finished$branch != "c", ]$xc)))
  expect_false(anyNA(finished$shared))
  # every subspace received its initial design
  expect_true(all(table(finished$branch)[c("a", "b", "c")] >= 2L))
  # points were proposed by the surrogate and the lambda of a worker is not decayed by default
  proposed = finished[!is.na(finished$acq_lambda), ]
  expect_data_table(proposed, min.rows = 1L)
  expect_equal(proposed$acq_lambda, proposed$acq_lambda_0)
  expect_r6(optimizer$surrogate, "SurrogateLearner")
})

test_that("OptimizerADBOThompson keeps the subspaces of one profile away from the workers of another", {
  profiles = c(gpu = 1, cpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = make_instance(rush, n_evals = 30L)
  optimizer = opt(
    "adbo_thompson",
    branch = "branch",
    subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
    design_size = 2L
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  worker_info = instance$rush$worker_info
  expect_set_equal(worker_info$profile, c("cpu", "gpu"))
  gpu_worker = worker_info[worker_info$profile == "gpu", ]$worker_id
  cpu_worker = worker_info[worker_info$profile == "cpu", ]$worker_id

  finished = instance$archive$data[instance$archive$data$state == "finished", ]
  # the gpu worker only ever evaluated subspace 'a', the cpu worker only 'b' and 'c'
  expect_equal(unique(finished[finished$worker_id == gpu_worker, ]$branch), "a")
  expect_set_equal(unique(finished[finished$worker_id == cpu_worker, ]$branch), c("b", "c"))
})

test_that("OptimizerADBOThompson evaluates the initial designs evenly in debug mode", {
  rush = rush::rsh(config = redis_configuration())
  old = options(bbotk.debug = TRUE)
  on.exit({
    rush$reset()
    options(old)
  })

  instance = make_instance(rush, n_evals = 9L)
  optimizer = opt(
    "adbo_thompson",
    branch = "branch",
    subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
    design_size = 3L,
    catch_errors = FALSE
  )
  optimizer$optimize(instance)

  # the single worker in the main process has no compute profile and samples among all subspaces
  finished = instance$archive$finished_data
  expect_equal(as.integer(table(finished$branch[1:9])[c("a", "b", "c")]), rep(3L, 3L))
  expect_true(all(is.na(finished$acq_cb[1:9])) || "acq_cb" %nin% names(finished))
  expect_false(optimizer$surrogate$param_set$values$catch_errors)
  expect_false(optimizer$acq_optimizer$param_set$values$catch_errors)
})

test_that("OptimizerADBOThompson counts a supplied initial design towards the design size", {
  rush = rush::rsh(config = redis_configuration())
  old = options(bbotk.debug = TRUE)
  on.exit({
    rush$reset()
    options(old)
  })

  instance = make_instance(rush, n_evals = 8L)
  design = generate_design_random(PS_1D_BRANCH3, n = 30L)$data[branch == "a"][1:2]
  optimizer = opt("adbo_thompson", branch = "branch", initial_design = design, design_size = 2L)
  optimizer$optimize(instance)

  finished = instance$archive$finished_data
  expect_equal(finished$xa[1:2], design$xa)
  # subspace 'a' is already covered, so only 'b' and 'c' receive random points
  expect_equal(sort(finished$branch[3:6]), c("b", "b", "c", "c"))
  expect_false(anyNA(finished$acq_cb[7:8]))
})

test_that("OptimizerADBOThompson checks the branching parameter and the compute profiles", {
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  expect_error(opt("adbo_thompson")$optimize(make_instance(rush, 5L)), "branch")
  expect_error(opt("adbo_thompson", branch = "xa")$optimize(make_instance(rush, 5L)), "categorical")
  expect_error(opt("adbo_thompson", branch = "foo")$optimize(make_instance(rush, 5L)), "categorical")

  optimizer = opt("adbo_thompson", branch = "branch", subspace_profiles = c(a = "cpu"))
  expect_error(optimizer$optimize(make_instance(rush, 5L)), "subspace_profiles")

  # the profiles must cover every profile a subspace is assigned to, but not more
  optimizer = opt(
    "adbo_thompson",
    branch = "branch",
    subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
    profiles = c(gpu = 1, cpu = 1, tpu = 1)
  )
  expect_error(optimizer$optimize(make_instance(rush, 5L)), "names of 'profiles'")
})

test_that("OptimizerADBOThompson evaluates a single-point subspace exactly once", {
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  search_space = ps(
    branch = p_fct(c("a", "b")),
    xb = p_dbl(-1, 1, depends = branch == "b")
  )
  # the parameter-free branch is the better one, so its arm keeps winning
  fun = function(xs) list(y = if (xs$branch == "a") -10 else xs$xb^2)
  objective = bbotk::ObjectiveRFun$new(fun = fun, domain = search_space, codomain = FUN_1D_CODOMAIN)
  instance = oi_async(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 15L),
    rush = rush
  )
  optimizer = opt("adbo_thompson", branch = "branch", design_size = 3L, n_workers = 1L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  finished = instance$archive$finished_data
  expect_equal(sum(finished$branch == "a"), 1L)
  expect_gte(sum(finished$branch == "b"), 14L)
})

test_that("OptimizerADBOThompson terminates the workers of exhausted subspaces", {
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  search_space = ps(
    branch = p_fct(c("a", "b")),
    fb = p_fct(c("lo", "hi"), depends = branch == "b"),
    lb = p_lgl(depends = branch == "b")
  )
  fun = function(xs) list(y = if (xs$branch == "a") 1 else 2)
  objective = bbotk::ObjectiveRFun$new(fun = fun, domain = search_space, codomain = FUN_1D_CODOMAIN)
  instance = oi_async(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 20L),
    rush = rush
  )
  optimizer = opt("adbo_thompson", branch = "branch", design_size = 2L, n_workers = 1L)

  # the worker runs out of configurations before the terminator triggers and the optimization ends nevertheless
  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  finished = instance$archive$finished_data
  expect_data_table(finished, nrows = 5L)
  expect_equal(uniqueN(finished[, c("branch", "fb", "lb")]), 5L)
  expect_false(instance$is_terminated)
})

test_that("OptimizerADBOThompson has the Thompson sampling parameters", {
  optimizer = opt("adbo_thompson")
  expect_r6(optimizer, c("OptimizerADBOThompson", "OptimizerADBO", "OptimizerAsyncMbo"))
  expect_equal(optimizer$param_set$values$top_quantile, 0.1)
  expect_equal(optimizer$param_set$values$alpha, 1)
  expect_equal(optimizer$param_set$values$beta, 1)
  expect_true(optimizer$param_set$values$catch_errors)
  # the lambda decay of OptimizerADBO is disabled by default
  expect_equal(optimizer$param_set$values$rate, 0)
  expect_subset(c("branch", "subspace_profiles", "lambda", "rate", "period"), optimizer$param_set$ids())
  expect_man_exists(optimizer$man)
})
