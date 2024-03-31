test_that("mlr_result_assigners", {
  expect_dictionary(mlr_result_assigners, min_items = 1L)
  keys = mlr_result_assigners$keys()

  for (key in keys) {
    a = mlr_result_assigners$get(key)
    expect_r6(a, classes = "ResultAssigner")
  }
})

test_that("as.data.table(mlr_result_assigners)", {
  d = as.data.table(mlr_result_assigners)
  expect_data_table(d)
  expect_character(d$key, unique = TRUE, any.missing = FALSE)
  expect_character(d$label, unique = TRUE, any.missing = FALSE)
  expect_character(d$man, unique = TRUE, any.missing = FALSE)
})

test_that("as.data.table(..., objects = TRUE)", {
  tab = as.data.table(mlr_result_assigners, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "ResultAssigner", any.missing = FALSE)
})

