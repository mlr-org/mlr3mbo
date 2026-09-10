test_that("partition_search_space splits a branching search space", {
  subspaces = partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = "b"))

  expect_list(subspaces, types = "ParamSet", len = 2L)
  expect_names(names(subspaces), identical.to = c("a", "b"))
  expect_set_equal(subspaces$a$ids(), c("branch", "xa", "shared"))
  expect_set_equal(subspaces$b$ids(), c("branch", "xb", "fb", "shared"))
  expect_equal(subspaces$a$levels$branch, "a")
  expect_equal(subspaces$b$levels$branch, "b")
})

test_that("partition_search_space groups multiple levels and drops transitive dependencies", {
  search_space = ps(
    learner = p_fct(c("gpu1", "cpu1", "cpu2")),
    gpu1.x = p_int(1, 16, depends = learner == "gpu1"),
    cpu1.x = p_dbl(0, 1, depends = learner == "cpu1"),
    cpu2.booster = p_fct(c("tree", "linear"), depends = learner == "cpu2"),
    cpu2.depth = p_int(1, 10, depends = cpu2.booster == "tree"),
    shared = p_dbl(0, 1)
  )

  subspaces = partition_search_space(
    search_space,
    param = "learner",
    groups = list(gpu = "gpu1", cpu = c("cpu1", "cpu2"))
  )

  expect_set_equal(subspaces$gpu$ids(), c("learner", "gpu1.x", "shared"))
  expect_set_equal(subspaces$cpu$ids(), c("learner", "cpu1.x", "cpu2.booster", "cpu2.depth", "shared"))
  expect_equal(subspaces$cpu$deps[get("id") == "cpu2.depth", ][["on"]], "cpu2.booster")
})

test_that("partition_search_space keeps trafos", {
  search_space = ps(
    learner = p_fct(c("a", "b")),
    a.x = p_dbl(1e-4, 1, logscale = TRUE, depends = learner == "a"),
    b.x = p_dbl(0, 1, depends = learner == "b")
  )

  subspaces = partition_search_space(search_space, param = "learner", groups = list(a = "a", b = "b"))

  expect_true(subspaces$a$has_trafo)
  expect_equal(subspaces$a$trafo(list(learner = "a", a.x = 0))$a.x, 1)
})

test_that("partition_search_space checks its arguments", {
  expect_error(
    partition_search_space(PS_1D_BRANCH, param = "shared", groups = list(a = "a")),
    "must be a 'ParamFct'"
  )
  expect_error(
    partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = c("a", "b"))),
    "must be disjoint"
  )
  expect_error(
    partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a")),
    "levels of the groups"
  )
})

test_that("subspace_contains separates the subspaces", {
  subspaces = partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = "b"))
  data = generate_design_random(PS_1D_BRANCH, n = 20L)$data

  expect_equal(subspace_contains(subspaces$a, data), data$branch == "a")
  expect_equal(subspace_contains(subspaces$b, data), data$branch == "b")
})

test_that("partition_search_space defaults to one subspace per level", {
  subspaces = partition_search_space(PS_1D_BRANCH, param = "branch")
  expect_names(names(subspaces), identical.to = c("a", "b"))
  expect_equal(subspaces, partition_search_space(PS_1D_BRANCH, param = "branch", groups = list(a = "a", b = "b")))
})

# a branch without parameters and a branch with two discrete parameters next to a numeric branch
PS_1D_DISCRETE = ps(
  branch = p_fct(c("a", "b", "c")),
  xa = p_dbl(-1, 1, depends = branch == "a"),
  fb = p_fct(c("lo", "hi"), depends = branch == "b"),
  lb = p_lgl(depends = branch == "b")
)

test_that("subspace_grid enumerates the configurations of discrete subspaces", {
  subspaces = partition_search_space(PS_1D_DISCRETE, param = "branch")
  expect_null(subspace_grid(subspaces$a))
  expect_data_table(subspace_grid(subspaces$b), nrows = 4L)
  expect_data_table(subspace_grid(subspaces$c), nrows = 1L)
  # dependencies are respected
  grouped = partition_search_space(PS_1D_DISCRETE, param = "branch", groups = list(a = "a", bc = c("b", "c")))
  expect_data_table(subspace_grid(grouped$bc), nrows = 5L)
  # large discrete subspaces are not materialized
  expect_null(subspace_grid(ps(x = p_int(1, 200), y = p_int(1, 200))))
})

test_that("subspace_grid enumerates bounded integers", {
  subspace = ps(branch = p_fct("a"), i = p_int(1, 8), l = p_lgl())
  grid = subspace_grid(subspace)
  expect_data_table(grid, nrows = 16L)
  expect_set_equal(grid$i, 1:8)
  expect_equal(uniqueN(grid), 16L)
  expect_data_table(generate_design_subspace(subspace, n = 5L), nrows = 5L)
  expect_true(subspace_exhausted(subspace, grid, n_configurations = 16L))
  # unbounded integers cannot be enumerated
  expect_null(subspace_grid(ps(i = p_int(1))))
  expect_null(subspace_grid(ps(i = p_int(1, 8, logscale = TRUE))))
})

test_that("subspace_exhausted detects fully evaluated subspaces", {
  subspaces = partition_search_space(PS_1D_DISCRETE, param = "branch")
  grid = subspace_grid(subspaces$b)
  expect_false(subspace_exhausted(subspaces$b, grid[1:3], n_configurations = 4))
  expect_true(subspace_exhausted(subspaces$b, grid, n_configurations = 4))
  # duplicates do not count
  expect_false(subspace_exhausted(subspaces$b, grid[c(1, 1, 2, 2)], n_configurations = 4))
  expect_true(subspace_exhausted(subspaces$c, data.table(branch = "c"), n_configurations = 1))
  expect_false(subspace_exhausted(subspaces$c, data.table(branch = character()), n_configurations = 1))
  # a subspace with numeric parameters is never exhausted
  expect_false(subspace_exhausted(subspaces$a, generate_design_random(subspaces$a, 100L)$data, n_configurations = Inf))
})

test_that("generate_design_subspace caps the design of discrete subspaces", {
  subspaces = partition_search_space(PS_1D_DISCRETE, param = "branch")
  expect_data_table(generate_design_subspace(subspaces$a, n = 10L), nrows = 10L)
  design = generate_design_subspace(subspaces$b, n = 10L)
  expect_data_table(design, nrows = 4L)
  expect_equal(uniqueN(design), 4L)
  expect_data_table(generate_design_subspace(subspaces$b, n = 2L), nrows = 2L)
  expect_data_table(generate_design_subspace(subspaces$c, n = 10L), nrows = 1L)
})
