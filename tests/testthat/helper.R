lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

# Simple 1D Functions
PS_1D = ParamSet$new(list(
  ParamDbl$new("x", lower = -1, upper = 1)
))
FUN_1D = function(xs) {
  list(y = as.numeric(xs)^2)
}
FUN_1D_CODOMAIN = ParamSet$new(list(ParamDbl$new("y", tags = c("minimize", "random_tag"))))
OBJ_1D = ObjectiveRFun$new(fun = FUN_1D, domain = PS_1D, codomain = FUN_1D_CODOMAIN, properties = "single-crit")

FUN_1D_2 = function(xs) {
  list(y1 = as.numeric(xs)^2, y2 = sqrt(abs(as.numeric(xs))))
}
FUN_1D_2_CODOMAIN = ParamSet$new(list(ParamDbl$new("y1", tags = c("minimize", "random_tag")), ParamDbl$new("y2", tags = c("minimize", "random_tag"))))
OBJ_1D_2 = ObjectiveRFun$new(fun = FUN_1D_2, domain = PS_1D, codomain = FUN_1D_2_CODOMAIN, properties = "multi-crit")

# Simple 1D Functions with noise
FUN_1D_NOISY = function(xs) {
  list(y = as.numeric(xs)^2 + rnorm(1, sd = 0.5))
}
OBJ_1D_NOISY = ObjectiveRFun$new(fun = FUN_1D_NOISY, domain = PS_1D, codomain = FUN_1D_CODOMAIN, properties = c("single-crit", "noisy"))

FUN_1D_2_NOISY = function(xs) {
  list(y1 = as.numeric(xs)^2 + rnorm(1, sd = 0.5), y2 = sqrt(abs(as.numeric(xs))) + rnorm(1, sd = 0.5))
}
OBJ_1D_2_NOISY = ObjectiveRFun$new(fun = FUN_1D_2, domain = PS_1D, codomain = FUN_1D_2_CODOMAIN, properties = c("multi-crit", "noisy"))

# Mixed 1D Functions
PS_1D_MIXED = ParamSet$new(list(
  ParamDbl$new("x1", -5, 5),
  ParamFct$new("x2", levels = c("a", "b", "c")),
  ParamInt$new("x3", 1L, 2L),
  ParamLgl$new("x4")
))
PS_1D_MIXED_DEPS = PS_1D_MIXED$clone(deep = TRUE)
PS_1D_MIXED_DEPS$add_dep("x2", on = "x4", cond = CondEqual$new(TRUE))

FUN_1D_MIXED = function(xs) {
  list(y = (xs$x1 - switch(xs$x2, "a" = 0, "b" = 1, "c" = 2)) %% xs$x3 + (if (xs$x4) xs$x1 else pi))
}
OBJ_1D_MIXED = ObjectiveRFun$new(fun = FUN_1D_MIXED, domain = PS_1D_MIXED, properties = "single-crit")
OBJ_1D_MIXED_DEPS = ObjectiveRFun$new(fun = FUN_1D_MIXED, domain = PS_1D_MIXED_DEPS, properties = "single-crit")

FUN_1D_2_MIXED = function(xs) {
  list(y1 = (xs$x1 - switch(xs$x2, "a" = 0, "b" = 1, "c" = 2)) %% xs$x3 + (if (xs$x4) xs$x1 else pi), y2 = xs$x1)
}
OBJ_1D_2_MIXED = ObjectiveRFun$new(fun = FUN_1D_2_MIXED, domain = PS_1D_MIXED, codomain = FUN_1D_2_CODOMAIN, properties = "multi-crit")


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

# Simple 2D Function with noise
FUN_2D_NOISY = function(xs) {
  y = sum(as.numeric(xs)^2) + rnorm(1, sd = 0.5)
  list(y = y)
}
OBJ_2D_NOISY = ObjectiveRFun$new(fun = FUN_2D_NOISY, domain = PS_2D_domain, properties = c("single-crit", "noisy"))

# Instance helper
MAKE_INST = function(objective = OBJ_2D, search_space = PS_2D, terminator = trm("evals", n_evals = 5)) {
  if (objective$codomain$length == 1) {
    OptimInstanceSingleCrit$new(objective = objective, search_space = search_space, terminator = terminator)
  } else {
    OptimInstanceMultiCrit$new(objective = objective, search_space = search_space, terminator = terminator)
  }

}

MAKE_INST_1D = function(terminator = trm("evals", n_evals = 5)) {
  MAKE_INST(objective = OBJ_1D, search_space = PS_1D, terminator = terminator)
}

MAKE_INST_1D_NOISY = function(terminator = trm("evals", n_evals = 5)) {
  MAKE_INST(objective = OBJ_1D_NOISY, search_space = PS_1D, terminator = terminator)
}

MAKE_DESIGN = function(instance, n = 4) {
  generate_design_lhs(instance$search_space, n)$data
}

library(mlr3learners)

REGR_KM_NOISY = lrn("regr.km", covtype = "matern3_2", nugget.stability = 10^-8)
REGR_KM_NOISY$encapsulate = c(train = "callr", predict = "callr")
REGR_KM_DETERM = lrn("regr.km", nugget.estim = TRUE, covtype = "matern3_2", jitter = 0.001)
REGR_KM_DETERM$encapsulate = c(train = "callr", predict = "callr")

SURR_KM_DETERM = SurrogateSingleCritLearner$new(learner = REGR_KM_DETERM)
SURR_KM_NOISY = SurrogateSingleCritLearner$new(learner = REGR_KM_NOISY)
SURR2D_KM_DETERM = SurrogateMultiCritLearners$new(learners = replicate(2, REGR_KM_DETERM))

ACQ_OPT_DEF = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000))
