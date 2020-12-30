test_that("AcqOptimizer param_set", {
  acqo = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L))
  expect_r6(acqo$param_set, "ParamSet")
  expect_true(all(c("fix_distance", "dist_threshold") %in% acqo$param_set$ids()))
  expect_r6(acqo$param_set$params$fix_distance, "ParamLgl")
  expect_r6(acqo$param_set$params$dist_threshold, "ParamDbl")
})

test_that("AcqOptimizer fix_xdt_distance", {
  # FIXME: use MIXED 1D helpers in #35
  domain = ParamSet$new(list(
    ParamDbl$new("x1", -5, 5),
    ParamFct$new("x2", levels = c("a", "b", "c")),
    ParamInt$new("x3", 1L, 2L),
    ParamLgl$new("x4"))
  )

  # FIXME: test for multiple redundant proposed points
  # FIXME: test logging?

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
  set.seed(1)
  dist_threshold = 1
  xdt_fixed = fix_xdt_distance(xdt_redundant, previous_xdt = previous_xdt, search_space = domain, dist_threshold = dist_threshold)
  check_gower_dist(get_gower_dist(xdt_fixed, previous_xdt), dist_threshold = dist_threshold)
})

