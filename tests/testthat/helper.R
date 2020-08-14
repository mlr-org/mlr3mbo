lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

# Simple 1D Function
PS_1D_domain = ParamSet$new(list(
  ParamDbl$new("x", lower = -5, upper = 5)
))

FUN_1D = function(xs) {
  list(y = as.numeric(xs)^2)
}

FUN_1D_CODOMAIN = ParamSet$new(list(ParamDbl$new("y", tags = "minimize")))

OBJ_1D = ObjectiveRFun$new(fun = FUN_1D, domain = PS_1D_domain,
  properties = "single-crit")

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
