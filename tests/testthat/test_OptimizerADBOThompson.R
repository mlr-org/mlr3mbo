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
SUBSPACES_1D_BRANCH3 = partition_search_space(PS_1D_BRANCH3, param = "branch")

test_that("OptimizerADBOThompson samples among the subspaces of a shared compute profile", {
  profiles = c(cpu = 2)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH3,
    search_space = PS_1D_BRANCH3,
    terminator = trm("evals", n_evals = 30L),
    rush = rush
  )
  optimizer = opt(
    "adbo_thompson",
    subspaces = SUBSPACES_1D_BRANCH3,
    subspace_profiles = c(a = "cpu", b = "cpu", c = "cpu"),
    design_size = 2L
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  data = instance$archive$data
  expect_data_table(data, min.rows = 30L)
  expect_names(names(data), must.include = c(".subspace", "acq_cb", "acq_lambda", "acq_lambda_0"))
  finished = data[data$state == "finished", ]
  # every point was proposed from the subspace it belongs to
  expect_equal(finished$.subspace, finished$branch)
  expect_true(all(is.na(finished[finished$branch != "a", ]$xa)))
  expect_true(all(is.na(finished[finished$branch != "b", ]$xb)))
  expect_true(all(is.na(finished[finished$branch != "c", ]$xc)))
  # the workers switched subspaces after the initial designs
  expect_set_equal(unique(finished$.subspace), c("a", "b", "c"))
  proposed = finished[-seq_len(6L), ]
  expect_gt(length(unique(proposed$.subspace)), 1L)
})

test_that("OptimizerADBOThompson keeps the subspaces of one profile away from the workers of another", {
  profiles = c(gpu = 1, cpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH3,
    search_space = PS_1D_BRANCH3,
    terminator = trm("evals", n_evals = 30L),
    rush = rush
  )
  optimizer = opt(
    "adbo_thompson",
    subspaces = SUBSPACES_1D_BRANCH3,
    subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
    design_size = 2L
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  worker_info = instance$rush$worker_info
  expect_set_equal(worker_info$profile, c("cpu", "gpu"))
  gpu_worker = worker_info[worker_info$profile == "gpu", ]$worker_id
  cpu_worker = worker_info[worker_info$profile == "cpu", ]$worker_id

  finished = instance$archive$data[instance$archive$data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch)
  # the gpu worker only ever evaluated subspace 'a', the cpu worker only 'b' and 'c'
  expect_equal(unique(finished[finished$worker_id == gpu_worker, ]$.subspace), "a")
  expect_subset(finished[finished$worker_id == cpu_worker, ]$.subspace, c("b", "c"))
  expect_set_equal(unique(finished$.subspace), c("a", "b", "c"))
})

test_that("OptimizerADBOThompson reduces to OptimizerADBOSubspaces with one subspace per profile", {
  profiles = c(a = 1, b = 1, c = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH3,
    search_space = PS_1D_BRANCH3,
    terminator = trm("evals", n_evals = 20L),
    rush = rush
  )
  optimizer = opt("adbo_thompson", subspaces = SUBSPACES_1D_BRANCH3, design_size = 2L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  worker_info = instance$rush$worker_info
  finished = instance$archive$data[instance$archive$data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch)
  # every worker stayed in the subspace of its profile
  assignment = unique(finished[, c("worker_id", ".subspace")])
  expect_equal(nrow(assignment), 3L)
  expect_equal(
    assignment$.subspace[match(worker_info$worker_id, assignment$worker_id)],
    worker_info$profile
  )
})

test_that("OptimizerADBOThompson evaluates the initial designs in debug mode", {
  rush = rush::rsh(config = redis_configuration())
  old = options(bbotk.debug = TRUE)
  on.exit({
    rush$reset()
    options(old)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH3,
    search_space = PS_1D_BRANCH3,
    terminator = trm("evals", n_evals = 12L),
    rush = rush
  )
  optimizer = opt(
    "adbo_thompson",
    subspaces = SUBSPACES_1D_BRANCH3,
    subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
    design_size = 2L
  )
  optimizer$optimize(instance)

  # the single worker in the main process has no compute profile and samples among all subspaces
  finished = instance$archive$finished_data
  expect_equal(finished$.subspace, finished$branch)
  expect_gte(sum(finished$.subspace == "a"), 2L)
  expect_gte(sum(finished$.subspace == "b"), 2L)
  expect_gte(sum(finished$.subspace == "c"), 2L)
})

test_that("OptimizerADBOThompson checks the compute profiles", {
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  make_instance = function() {
    oi_async(
      objective = OBJ_1D_BRANCH3,
      search_space = PS_1D_BRANCH3,
      terminator = trm("evals", n_evals = 5L),
      rush = rush
    )
  }

  optimizer = opt("adbo_thompson", subspaces = SUBSPACES_1D_BRANCH3, subspace_profiles = c(a = "cpu"))
  expect_error(optimizer$optimize(make_instance()), "subspace_profiles")

  # the profiles must cover every profile a subspace is assigned to, but not more
  optimizer = opt(
    "adbo_thompson",
    subspaces = SUBSPACES_1D_BRANCH3,
    subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
    profiles = c(gpu = 1, cpu = 1, tpu = 1)
  )
  expect_error(optimizer$optimize(make_instance()), "names of 'profiles'")

  optimizer = opt("adbo_thompson", subspaces = SUBSPACES_1D_BRANCH3, n_workers = 1L)
  expect_error(optimizer$optimize(make_instance()), "Set 'profiles' instead of 'n_workers'")

  optimizer = opt("adbo_thompson", subspaces = SUBSPACES_1D_BRANCH3)
  expect_error(optimizer$optimize(make_instance()), "divides the workers")
})

test_that("OptimizerADBOThompson evaluates a single-point subspace exactly once", {
  profiles = c(cpu = 2)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
  subspaces = partition_search_space(search_space, param = "branch")
  optimizer = opt("adbo_thompson", subspaces = subspaces, subspace_profiles = c(a = "cpu", b = "cpu"), design_size = 3L)

  # the generated design of the single-point subspace is capped at its one configuration
  designs = get_private(optimizer)$.generate_designs(subspaces)
  expect_data_table(designs$a, nrows = 1L)
  expect_data_table(designs$b, nrows = 3L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  finished = instance$archive$finished_data
  expect_equal(sum(finished$branch == "a"), 1L)
  expect_gte(sum(finished$branch == "b"), 14L)
})

test_that("OptimizerADBOThompson terminates the workers of exhausted subspaces", {
  profiles = c(cpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
  optimizer = opt(
    "adbo_thompson",
    subspaces = partition_search_space(search_space, param = "branch"),
    subspace_profiles = c(a = "cpu", b = "cpu"),
    design_size = 2L
  )

  # the worker runs out of configurations before the terminator triggers and the optimization ends nevertheless
  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  finished = instance$archive$finished_data
  expect_data_table(finished, nrows = 5L)
  expect_equal(uniqueN(finished[, c("branch", "fb", "lb")]), 5L)
  expect_false(instance$is_terminated)
})

test_that("OptimizerADBOThompson has the Thompson sampling parameters", {
  optimizer = opt("adbo_thompson")
  expect_r6(optimizer, c("OptimizerADBOThompson", "OptimizerADBOSubspaces", "OptimizerAsyncMbo"))
  expect_equal(optimizer$param_set$values$top_quantile, 0.1)
  expect_equal(optimizer$param_set$values$alpha, 1)
  expect_equal(optimizer$param_set$values$beta, 1)
  expect_subset(c("subspaces", "subspace_profiles", "lambda", "rate", "period"), optimizer$param_set$ids())
  expect_man_exists(optimizer$man)
})
