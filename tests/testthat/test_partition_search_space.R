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
