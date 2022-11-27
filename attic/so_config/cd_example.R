library(bbotk)
library(paradox)
library(R6)
library(checkmate)
library(mlr3misc)


source("OptimizerCoordinateDescent.R")

domain = ps(x1 = p_dbl(lower = -1, upper = 1),
            x2 = p_fct(c("a", "b", "c")),
            x3 = p_int(lower = 1, upper = 3, depends = x2 == "a"),
            x4 = p_lgl(),
            x6 = p_dbl(lower = 0, upper = 10, depends = (x2 == "b" && x4 == TRUE)),
            x7 = p_fct(c("A", "B"), depends = x2 %in% c("a", "b")))

objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    x3 = xdt$x3
    x3[is.na(x3)] = 1
    x6 = xdt$x6
    x6[is.na(x6)] = 0
    y = x3 * xdt$x1 - x6
    data.table(y = y)
  },
  domain = domain,
  codomain = ps(y = p_dbl(tags = "minimize")))

instance = OptimInstanceSingleCrit$new(objective = objective, terminator = trm("none"))
optimizer = OptimizerCoordinateDescent$new()
optimizer$optimize(instance)
