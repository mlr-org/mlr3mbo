skip_if_not_installed("rush")
skip_if_no_redis()

SUBSPACES_1D_BRANCH = partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = "b"))

# every row of the design of a subspace must show up in the archive
expect_design_evaluated = function(design, data) {
  expect_equal(nrow(data[design, on = names(design), nomatch = NULL]), nrow(design))
}

test_that("OptimizerADBOSubspaces keeps every worker in its subspace", {
  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
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
    n_workers_subspace = c(a = 1L, b = 1L),
    design_size = 3L
  )

  expect_data_table(optimizer$optimize(instance), nrows = 1L)

  data = instance$archive$data
  expect_data_table(data, min.rows = 20L)
  expect_names(names(data), must.include = c(".subspace", "acq_cb", "acq_lambda", "acq_lambda_0"))
  # every point was proposed from the subspace it belongs to
  expect_equal(data$.subspace, data$branch)
  expect_true(all(is.na(data[data$branch == "a", "xb"][[1L]])))
  expect_true(all(is.na(data[data$branch == "b", "xa"][[1L]])))
  # both subspaces were worked on
  expect_set_equal(unique(data$.subspace), c("a", "b"))
})

test_that("OptimizerADBOSubspaces evaluates one initial design per subspace", {
  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
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
    n_workers_subspace = c(a = 1L, b = 1L),
    design_size = 2L,
    design_size_subspace = c(b = 4L)
  )
  optimizer$optimize(instance)

  data = instance$archive$data
  designs = get_private(optimizer)$.designs
  expect_data_table(designs$a, nrows = 2L)
  expect_data_table(designs$b, nrows = 4L)
  expect_design_evaluated(designs$a, data)
  expect_design_evaluated(designs$b, data)
})

test_that("OptimizerADBOSubspaces accepts an initial design per subspace and a worker setup", {
  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
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
    n_workers_subspace = c(a = 1L, b = 1L),
    initial_design_subspace = initial_design_subspace,
    worker_setup = function(subspace_id) Sys.setenv(MLR3MBO_TEST_SUBSPACE = subspace_id)
  )
  optimizer$optimize(instance)

  data = instance$archive$data
  expect_data_table(data, min.rows = 12L)
  expect_design_evaluated(initial_design_subspace$a, data)
  expect_design_evaluated(initial_design_subspace$b, data)
})

test_that("OptimizerADBOSubspaces checks the subspaces", {
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

  optimizer = opt("adbo_subspaces", subspaces = list(a = ps(nope = p_dbl(0, 1))))
  expect_error(optimizer$optimize(make_instance()), "not part of the search")

  optimizer = opt("adbo_subspaces", subspaces = list(a = PS_1D_BRANCH, b = PS_1D_BRANCH))
  expect_error(optimizer$optimize(make_instance()), "must be disjoint")

  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    n_workers_subspace = c(a = 1L, wrong = 1L)
  )
  expect_error(optimizer$optimize(make_instance()), "n_workers_subspace")

  optimizer = opt(
    "adbo_subspaces",
    subspaces = SUBSPACES_1D_BRANCH,
    initial_design = generate_design_random(PS_1D_BRANCH, n = 2L)$data
  )
  expect_error(optimizer$optimize(make_instance()), "initial_design_subspace")
})
