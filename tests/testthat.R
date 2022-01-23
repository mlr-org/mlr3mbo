if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(mlr3mbo)
  library(mlr3learners)
  test_check("mlr3mbo")
}
