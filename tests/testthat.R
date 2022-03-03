if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(mlr3mbo)
  library(mlr3)
  library(mlr3misc)
  library(mlr3learners)
  library(checkmate)
  test_check("mlr3mbo")
}
