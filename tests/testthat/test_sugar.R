test_that("SurrogateLearner sugar", {
  surrogate = srlrn(REGR_FEATURELESS, catch_errors = FALSE)
  expect_r6(surrogate, classes = "SurrogateLearner")
  expect_false(surrogate$param_set$values$catch_errors)
})

test_that("SurrogateLearnerCollection sugar", {
  surrogate = srlrn(list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), catch_errors = FALSE)
  expect_r6(surrogate, classes = "SurrogateLearnerCollection")
  expect_false(surrogate$param_set$values$catch_errors)
})

test_that("AcqFunction sugar", {
  acqfunction = acqf("cb", lambda = 3)
  expect_acqfunction(acqfunction)
  expect_equal(acqfunction$constants$values$lambda, 3)
})

test_that("AcqOptimizer sugar", {
  acqoptimizer = acqo(opt("random_search"), trm("evals"), catch_errors = FALSE)
  expect_r6(acqoptimizer, "AcqOptimizer")
  expect_false(acqoptimizer$param_set$values$catch_errors)
})

test_that("ResultAssigner sugar", {
  resultassigner = ras("surrogate")
  expect_r6(resultassigner, "ResultAssigner")
})

