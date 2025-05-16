if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(checkmate)
  library(mlr3mbo)
  test_check("mlr3mbo")
}
