test_that("default bayesopt_eips", {
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x ^ 2, time = xdt$x + 10),
    domain = PS_1D,
    codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time"))
  )
  instance = OptimInstanceSingleCrit$new(objective = objective, search_space = PS_1D, terminator = trm("evals", n_evals = 5L))
  bayesopt_eips(instance)
  expect_true(nrow(instance$archive$data) == 5L)
})
