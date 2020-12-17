test_that("xdt_fix_dist", {
  set.seed(1)
  acqo = AcqOptimizerRandomSearch$new()  # FIXME: test AcqOptimizerR directly once param_set is in superclass
  expect_r6(acqo$param_set, "ParamSet")
  expect_true("dist_threshold" %in% acqo$param_set$ids())
  expect_r6(acqo$param_set$params$dist_threshold, "ParamDbl")

  obfun = ObjectiveRFun$new(
  fun = function(xs) {
    list((xs$x1 - switch(xs$x2, "a" = 0, "b" = 1, "c" = 2)) %% xs$x3 + (if (xs$x4) xs$x1 else pi))
  },
  domain = ParamSet$new(list(
    ParamDbl$new("x1", -5, 5),
    ParamFct$new("x2", levels = c("a", "b", "c")),
    ParamInt$new("x3", 1L, 2L),
    ParamLgl$new("x4"))),
  id = "test"
  )

  # FIXME: make this more clear
  # FIXME: test logging?
  # FIXME: test for multiple redundant proposed points

  ### single point proposal to multiple previous points
  previous_xdt = data.table(x1 = c(0.1, 0.2, 0.3, 0.4), x2 = c("a", "b", "c", "a"), x3 = c(1L, 2L, 1L, 2L), x4 = c(TRUE, TRUE, FALSE, FALSE))
  xdt_ok = data.table(x1 = 0.5, x2 = "b", x3 = 1L, x4 = TRUE)
  expect_equal(address(acqo$xdt_fix_dist(xdt_ok, previous_xdt = previous_xdt, search_space = obfun$domain)), address(xdt_ok))  # no change at all, not even a copy
  xdt_redundant = previous_xdt[1L, ]
  xdt_fixed = acqo$xdt_fix_dist(xdt_redundant, previous_xdt = previous_xdt, search_space = obfun$domain)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 1L)
  suppressWarnings(expect_true(gower::gower_dist(xdt_fixed, xdt_redundant) > acqo$param_set$values$dist_threshold))

  ### single point proposal to single previous point
  expect_equal(address(acqo$xdt_fix_dist(xdt_ok, previous_xdt = previous_xdt[4L, ], search_space = obfun$domain)), address(xdt_ok))  # no change at all, not even a copy
  xdt_redundant = previous_xdt[4L, ]
  xdt_fixed = acqo$xdt_fix_dist(xdt_redundant, previous_xdt = previous_xdt[4L, ], search_space = obfun$domain)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 1L)
  suppressWarnings(expect_true(gower::gower_dist(xdt_fixed, xdt_redundant) > acqo$param_set$values$dist_threshold))

  # multiple point proposal to multiple previous points
  xdt_ok = rbind(xdt_ok, data.table(x1 = 0.6, x2 = "c", x3 = 2L, x4 = FALSE))
  expect_equal(address(acqo$xdt_fix_dist(xdt_ok, previous_xdt = previous_xdt, search_space = obfun$domain)), address(xdt_ok))  # no change at all, not even a copy
  xdt_redundant = rbind(xdt_ok, previous_xdt[3:4, ])
  xdt_fixed = acqo$xdt_fix_dist(xdt_redundant, previous_xdt = previous_xdt, search_space = obfun$domain)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 4L)
  expect_true(identical(xdt_fixed[1:2, ], xdt_redundant[1:2, ]))
  suppressWarnings(expect_true(all(map_lgl(seq_len(nrow(xdt_fixed)), function(i) all(gower::gower_dist(xdt_fixed[i, ], previous_xdt) > acqo$param_set$values$dist_threshold)))))

  # multiple point proposal to single previous point
  expect_equal(address(acqo$xdt_fix_dist(xdt_ok, previous_xdt = previous_xdt[4L, ], search_space = obfun$domain)), address(xdt_ok))  # no change at all, not even a copy
  xdt_fixed = acqo$xdt_fix_dist(xdt_redundant, previous_xdt = previous_xdt[4L, ], search_space = obfun$domain)
  expect_data_table(xdt_fixed, any.missing = FALSE, nrows = 4L)
  expect_true(identical(xdt_fixed[1:3, ], xdt_redundant[1:3, ]))
  suppressWarnings(expect_true(all(map_lgl(seq_len(nrow(xdt_fixed)), function(i) all(gower::gower_dist(xdt_fixed[i, ], previous_xdt[4L, ]) > acqo$param_set$values$dist_threshold)))))
})
