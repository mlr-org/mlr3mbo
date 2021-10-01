devtools::load_all()
library(bbotk)
library(paradox)

fun = function(xdt) {
  a = 20
  b = 0.2
  c = 2 * pi
  y = - a * exp(- b * sqrt((1 / 2) * rowSums(xdt^2))) -
    exp((1 / 2) * rowSums(cos(c * xdt))) + a + exp(1)
  data.table(y = y)
}
domain = ps(x1 = p_dbl(lower = -33, upper = 33), x2 = p_dbl(lower = -33, upper = 33))
codomain = ps(y = p_dbl(tags = "minimize"))
objective = ObjectiveRFunDt$new(fun = fun, domain = domain, codomain = codomain)

instance = OptimInstanceSingleCrit$new(objective, terminator = trm("evals", n_evals = 100))
optimizer = OptimizerMbo$new()
optimizer$optimize(instance)

library(ggplot2)

dat = generate_design_grid(domain, resolution = 300)$data
dat[, y:= objective$eval_dt(dat)]

p1 = ggplot(aes(x1, x2, z = y), data = dat) +
  geom_contour(colour = "black") +
  geom_point(aes(x1, x2, z = y, colour = batch_nr), data = instance$archive$data) +
  geom_point(aes(x1, x2, z = y), colour = "red", data = instance$archive$best()) +
  geom_point(aes(x1, x2, z = y), colour = "green", data = instance$archive$data[batch_nr == 1])

###
instance = OptimInstanceSingleCrit$new(objective, terminator = trm("evals", n_evals = 100))

bo_gp_ei = OptimizerMbo$new()
bo_rf_ei = OptimizerMbo$new(acq_function = default_acqfun(instance, default_surrogate(instance, learner = lrn("regr.ranger"))))

res = map_dtr(1:30, function(i) {
  instance$archive$clear()
  bo_gp_ei$optimize(instance)
  tmp1 = instance$archive$data[, c("y", "batch_nr")]
  tmp1[, method := "gp"]
  instance$archive$clear()
  bo_rf_ei$optimize(instance)
  tmp2 = instance$archive$data[, c("y", "batch_nr")]
  tmp2[, method := "rf"]
  tmp = rbind(tmp1, tmp2)
  tmp[, repl := i]
})

res[, best := cummin(y), by = .(method, repl)]
agg = res[, .(mb = mean(best), sdb = sd(best), n = length(best)), by = .(batch_nr, method)]
agg[, seb := sdb / sqrt(n) ]

ggplot(aes(x = batch_nr, y = mb, colour = method, fill = method), data = agg) +
  geom_line() +
  geom_ribbon(aes(ymin = mb - seb, ymax = mb + seb) , colour = NA, alpha = 0.25)

