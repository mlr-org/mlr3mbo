lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

# Simple 1D Function
PS_1D = ParamSet$new(list(
  ParamDbl$new("x", lower = -1, upper = 1)
))
FUN_1D = function(xs) {
  list(y = as.numeric(xs)^2)
}
FUN_1D_CODOMAIN = ParamSet$new(list(ParamDbl$new("y", tags = c("minimize", "random_tag"))))
OBJ_1D = ObjectiveRFun$new(fun = FUN_1D, domain = PS_1D, properties = "single-crit")

# Simple 2D Function
PS_2D_domain = ParamSet$new(list(
  ParamDbl$new("x1", lower = -1, upper = 1),
  ParamDbl$new("x2", lower = -1, upper = 1),
  ParamUty$new("foo") # the domain of the function should not matter.
))
PS_2D = ParamSet$new(list(
  ParamDbl$new("x1", lower = -1, upper = 1),
  ParamDbl$new("x2", lower = -1, upper = 1)
))
FUN_2D = function(xs) {
  y = sum(as.numeric(xs)^2)
  list(y = y)
}
FUN_2D_CODOMAIN = ParamSet$new(list(ParamDbl$new("y", tags = c("minimize", "random_tag"))))
OBJ_2D = ObjectiveRFun$new(fun = FUN_2D, domain = PS_2D_domain, properties = "single-crit")

# Instance helper
MAKE_INST = function(objective = OBJ_2D, search_space = PS_2D,
                     terminator = 5L) {
  if (is.integer(terminator)) {
    tt = TerminatorEvals$new()
    tt$param_set$values$n_evals = terminator
    terminator = tt
  }
  if (objective$codomain$length == 1) {
    OptimInstanceSingleCrit$new(objective = objective, search_space = search_space, terminator = terminator)
  } else {
    OptimInstanceMultiCrit$new(objective = objective, search_space = search_space, terminator = terminator)
  }

}

MAKE_INST_1D = function(terminator) {
  MAKE_INST(objective = OBJ_1D, search_space = PS_1D, terminator = terminator)
}
