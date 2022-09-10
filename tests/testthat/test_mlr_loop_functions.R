test_that("mlr_loop_functions", {
  expect_dictionary_loop_function(mlr_loop_functions, min_items = 1L)
  keys = mlr_loop_functions$keys()

  for (key in keys) {
    l = mlr_loop_functions$get(key)
    expect_class(l, classes = "loop_function")
  }
})

test_that("as.data.table(mlr_loop_functions)", {
  d = as.data.table(mlr_loop_functions)
  expect_data_table(d)
  expect_character(d$key, unique = TRUE, any.missing = FALSE)
  expect_character(d$label, unique = TRUE, any.missing = FALSE)
  expect_character(d$man, unique = TRUE, any.missing = FALSE)
})

test_that("as.data.table(..., objects = TRUE)", {
  tab = as.data.table(mlr_loop_functions, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "loop_function", any.missing = FALSE)
})
