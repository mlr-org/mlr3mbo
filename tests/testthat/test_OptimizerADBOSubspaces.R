skip_if_not_installed("rush")
skip_if_no_redis()

SUBSPACES_1D_BRANCH = partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = "b"))

# every row of the design of a subspace must show up in the archive
expect_design_evaluated = function(design, data) {
  expect_equal(nrow(data[design, on = names(design), nomatch = NULL]), nrow(design))
}

test_that("OptimizerADBOSubspaces keeps every worker in its subspace", {
  profiles = c(a = 1, b = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 20L),
    rush = rush
  )
  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, design_size = 3L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  data = instance$archive$data
  expect_data_table(data, min.rows = 20L)
  expect_names(names(data), must.include = c(".subspace", "acq_cb", "acq_lambda", "acq_lambda_0"))
  expect_true(all(is.na(data[data$branch == "a", "xb"][[1L]])))
  expect_true(all(is.na(data[data$branch == "b", "xa"][[1L]])))
  # every point was proposed from the subspace it belongs to;
  # `.subspace` is written when an evaluation finishes, so pending evaluations still have `NA`
  finished = data[data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch)
  # both subspaces were worked on
  expect_set_equal(unique(finished$.subspace), c("a", "b"))
})

test_that("OptimizerADBOSubspaces queues one initial design per subspace", {
  rush = rush::rsh(config = redis_configuration())
  on.exit(rush$reset())

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 12L),
    rush = rush
  )
  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    design_size = 2L,
    design_size_subspace = c(b = 4L)
  )
  get_private(optimizer)$.push_designs(instance, SUBSPACES_1D_BRANCH, c(a = "gpu", b = "cpu"))

  # the design of a subspace is queued for the compute profile of that subspace, the shared queue stays empty
  expect_equal(instance$archive$n_queued_per_profile, c(default = 0L, cpu = 4L, gpu = 2L))

  queued = instance$archive$queued_data
  expect_equal(queued[profile == "gpu", .subspace], rep("a", 2L))
  expect_equal(queued[profile == "cpu", .subspace], rep("b", 4L))
  # the points are padded, so the parameters outside of the subspace are inactive
  expect_true(all(is.na(queued[profile == "gpu", xb])))
  expect_true(all(is.na(queued[profile == "cpu", xa])))
})

test_that("OptimizerADBOSubspaces evaluates one initial design per subspace", {
  profiles = c(a = 1, b = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 12L),
    rush = rush
  )
  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    design_size = 2L,
    design_size_subspace = c(b = 4L)
  )
  optimizer$optimize(instance)

  # the whole queue was worked off, so every design point was evaluated by a worker of its subspace
  expect_equal(instance$archive$n_queued, 0L)
  finished = instance$archive$finished_data
  expect_gte(sum(finished$.subspace == "a"), 2L)
  expect_gte(sum(finished$.subspace == "b"), 4L)
})

test_that("OptimizerADBOSubspaces evaluates the initial designs in debug mode", {
  rush = rush::rsh(config = redis_configuration())
  old = options(bbotk.debug = TRUE)
  on.exit({
    rush$reset()
    options(old)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 8L),
    rush = rush
  )
  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, design_size = 2L)
  optimizer$optimize(instance)

  # the single worker in the main process has no compute profile, so the designs go to the shared queue
  finished = instance$archive$finished_data
  expect_gte(sum(finished$.subspace == "a"), 2L)
  expect_gte(sum(finished$.subspace == "b"), 2L)
})

test_that("OptimizerADBOSubspaces accepts an initial design per subspace", {
  profiles = c(a = 1, b = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 12L),
    rush = rush
  )
  initial_design_subspace = list(
    a = generate_design_random(SUBSPACES_1D_BRANCH$a, n = 2L)$data,
    b = generate_design_random(SUBSPACES_1D_BRANCH$b, n = 2L)$data
  )
  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    initial_design_subspace = initial_design_subspace
  )
  optimizer$optimize(instance)

  data = instance$archive$data
  expect_data_table(data, min.rows = 12L)
  expect_design_evaluated(initial_design_subspace$a, data)
  expect_design_evaluated(initial_design_subspace$b, data)
})

test_that("OptimizerADBOSubspaces respects a pre-set surrogate", {
  profiles = c(a = 1, b = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 12L),
    rush = rush
  )
  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, design_size = 2L)
  surrogate = default_surrogate(instance, force_random_forest = TRUE)
  surrogate$param_set$set_values(catch_errors = FALSE)
  optimizer$surrogate = surrogate

  optimizer$optimize(instance)

  expect_identical(optimizer$surrogate, surrogate)
  expect_false(optimizer$surrogate$param_set$values$catch_errors)
})

test_that("OptimizerADBOSubspaces pins subspaces to compute profiles", {
  profiles = c(cpu = 1, gpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 20L),
    rush = rush
  )
  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    subspace_profiles = c(a = "gpu", b = "cpu"),
    design_size = 3L
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  worker_info = instance$rush$worker_info
  expect_set_equal(worker_info$profile, c("cpu", "gpu"))

  finished = instance$archive$data[instance$archive$data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch)
  expect_set_equal(unique(finished$.subspace), c("a", "b"))
})

test_that("OptimizerADBOSubspaces distributes the workers with the profiles parameter", {
  profiles = c(a = 1, b = 2)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  instance = oi_async(
    objective = OBJ_1D_BRANCH,
    search_space = PS_1D_BRANCH,
    terminator = trm("evals", n_evals = 20L),
    rush = rush
  )
  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, profiles = profiles, design_size = 3L)

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  worker_info = instance$rush$worker_info
  expect_equal(sort(worker_info$profile), c("a", "b", "b"))

  finished = instance$archive$data[instance$archive$data$state == "finished", ]
  expect_equal(finished$.subspace, finished$branch)
  expect_set_equal(unique(finished$.subspace), c("a", "b"))
})

test_that("OptimizerADBOSubspaces checks the subspaces", {
  profiles = c(a = 1, b = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
  })

  make_instance = function() {
    oi_async(
      objective = OBJ_1D_BRANCH,
      search_space = PS_1D_BRANCH,
      terminator = trm("evals", n_evals = 5L),
      rush = rush
    )
  }

  optimizer = opt("adbo_subspaces", subspaces = list(a = ps(nope = p_dbl(0, 1))))
  expect_error(optimizer$optimize(make_instance()), "not part of the search")

  optimizer = opt("adbo_subspaces", subspaces = list(a = PS_1D_BRANCH, b = PS_1D_BRANCH))
  expect_error(optimizer$optimize(make_instance()), "must be disjoint")

  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    initial_design = generate_design_random(PS_1D_BRANCH, n = 2L)$data
  )
  expect_error(optimizer$optimize(make_instance()), "initial_design_subspace")
})

test_that("OptimizerADBOSubspaces checks the compute profiles", {
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  make_instance = function() {
    oi_async(
      objective = OBJ_1D_BRANCH,
      search_space = PS_1D_BRANCH,
      terminator = trm("evals", n_evals = 5L),
      rush = rush
    )
  }

  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, subspace_profiles = c(a = "cpu"))
  expect_error(optimizer$optimize(make_instance()), "subspace_profiles")

  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    subspace_profiles = c(a = "cpu", b = "cpu")
  )
  expect_error(optimizer$optimize(make_instance()), "must run on its own profile")

  # the workers are divided among the subspaces by the compute profiles, `n_workers` is not enough
  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, n_workers = 1L)
  expect_error(optimizer$optimize(make_instance()), "Set 'profiles' instead of 'n_workers'")

  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH)
  expect_error(optimizer$optimize(make_instance()), "divides the workers")

  optimizer = opt("adbo_subspaces", subspaces = SUBSPACES_1D_BRANCH, profiles = c(a = 1, cpu = 1))
  expect_error(optimizer$optimize(make_instance()), "names of 'profiles'")
})
