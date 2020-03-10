library(mlr3)
library(paradox)
# library(bbotk)
# roxygenize()
load_all("~/cos/bbotk")
load_all()

ps1 = ParamDbl$new("x", lower = -1, upper = 1)$rep(2)

fn = function(dt) {
  y = map_dbl(seq_row(dt), function(i) {
    x = dt[i,]
    sum(x^2)
  })
  data.table(y = y)
}

obj = ObjectiveSO$new(fun = fn, domain = ps1, minimize = TRUE)
term = TerminatorEvals$new()
term$param_set$values$n_evals = 9


ll = lrn("regr.rpart")
acqf = AcqFunctionMean$new(obj)
acqf_optim = AcqOptimizer$new()

a = bayesop_soo(obj, ll, acqf, term)
print(a)

