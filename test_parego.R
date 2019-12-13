library(mlr3)
library(paradox)
devtools::load_all()


f1 = function(x) sum(unlist(x)^2)
f2 = function(x) sum(unlist(x)^2-1)
f = function(x) c(f1(x), f2(x))
ps = ParamDbl$new("x", lower = -1, upper = 1)$rep(2)
te = TerminatorEvals$new()
te$param_set$values$n_evals = 12
obj = Objective$new(fun = f, param_set = ps, te)

ll = lrn("regr.rpart")
acqf = AcqFunctionMean$new()
acqf_optim = AcqOptimizer$new()

bayesop_soo(obj, ll, acqf, acqf_optim)

