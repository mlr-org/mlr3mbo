test_that("mlr_acqfunctions", {
  expect_dictionary(mlr_acqfunctions, min_items = 1L)
  keys = mlr_acqfunctions$keys()

  for (key in keys) {
    if (key == "multi") {
      a1 = AcqFunctionMean$new()
      a2 = AcqFunctionSD$new()
      a = mlr_acqfunctions$get(key, acq_functions = list(a1, a2))
    } else {
      a = mlr_acqfunctions$get(key)
      expect_r6(a, classes = "AcqFunction")
    }
  }
})

test_that("as.data.table(mlr_acqfunctions)", {
  d = as.data.table(mlr_acqfunctions)
  expect_data_table(d)
  expect_character(d$key, unique = TRUE, any.missing = FALSE)
  expect_character(d$label, unique = TRUE, any.missing = FALSE)
  expect_character(d$man, unique = TRUE, any.missing = FALSE)
})

test_that("as.data.table(..., objects = TRUE)", {
  tab = as.data.table(mlr_acqfunctions, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "AcqFunction", any.missing = FALSE)
})

