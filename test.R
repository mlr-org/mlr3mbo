library(mlr3)
library(paradox)
devtools::load_all()


f = function(x) sum(unlist(x)^2)
ps = ParamDbl$new("x", lower = -1, upper = 1)$rep(2)
te = TerminatorEvals$new()
te$param_set$values$n_evals = 12
obj = ObjectiveSO$new(fun = f, param_set = ps, te)

ll = lrn("regr.rpart")
acqf = AcqFunctionMean$new()
acqf_optim = AcqOptimizer$new()

bayesop_soo(obj, ll, acqf, acqf_optim)
