devtools::load_all()
library(ggplot2)
library(pammtools)

objective_function = function(xs) {
  list(y = 418.9829 * 2 - (sum(unlist(xs) * sin(sqrt(abs(unlist(xs)))))))
}
domain = ps(x1 = p_dbl(lower = -500, upper = 500),
  x2 = p_dbl(lower = -500, upper = 500))
codomain = ps(y = p_dbl(tags = "minimize"))

objective = ObjectiveRFun$new(
  fun = objective_function,
  domain = domain,
  codomain = codomain)

instance = OptimInstanceSingleCrit$new(
  objective = objective,
  search_space = domain,
  terminator = trm("evals", n_evals = 30))

res = map_dtr(1:10, function(r) {
  instance$archive$clear()
  set.seed(r)
  design = generate_design_random(instance$search_space, 10)$data
  instance$eval_batch(design)
  
  # Gaussian Process, EI, DIRECT
  surrogate = srlrn(lrn("regr.km",
    covtype = "matern3_2",
    optim.method = "gen",
    nugget.stability = 10^-8, control = list(trace = FALSE)))
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
    terminator = trm("stagnation", threshold = 1e-8))
  optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  dat1 = instance$archive$data
  dat1[, best := cummin(y)]
  dat1[, method := "ei"]
  dat1[, repl := r]
  
  instance$archive$clear()
  instance$eval_batch(design)
  # Gaussian Process, EI, DIRECT
  surrogate = srlrn(lrn("regr.km",
    covtype = "matern3_2",
    optim.method = "gen",
    nugget.stability = 10^-8, control = list(trace = FALSE)))
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
    terminator = trm("stagnation", threshold = 1e-8))
  acq_function = acqf("ttei", toplvl_acq_optimizer = acq_optimizer)
  optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  dat2 = instance$archive$data
  dat2[, best := cummin(y)]
  dat2[, method := "ttei"]
  dat2[, repl := r]

  instance$archive$clear()
  instance$eval_batch(design)
  optimizer = opt("random_search", batch_size = 1)
  optimizer$optimize(instance)
  dat3 = instance$archive$data
  dat3[, best := cummin(y)]
  dat3[, method := "rs"]
  dat3[, repl := r]

  instance$archive$clear()
  instance$eval_batch(design)
  # Gaussian Process, EI, DIRECT
  surrogate = srlrn(lrn("regr.ranger", num.trees = 2000L, se.method = "jack", keep.inbag = TRUE))
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
    terminator = trm("stagnation", threshold = 1e-8))
  optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  dat4 = instance$archive$data
  dat4[, best := cummin(y)]
  dat4[, method := "ei_rf"]
  dat4[, repl := r]

  instance$archive$clear()
  instance$eval_batch(design)
  # Gaussian Process, EI, DIRECT
  surrogate = srlrn(lrn("regr.ranger", num.trees = 2000L, se.method = "jack", keep.inbag = TRUE))
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"),
    terminator = trm("stagnation", threshold = 1e-8))
  acq_function = acqf("ttei", toplvl_acq_optimizer = acq_optimizer)
  optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  dat5 = instance$archive$data
  dat5[, best := cummin(y)]
  dat5[, method := "ttei_rf"]
  dat5[, repl := r]

  dat = rbind(dat1, dat2, dat3, dat4, dat5, fill = TRUE)
  dat
}, .fill = TRUE)

agg = res[, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(batch_nr, method)]

ggplot(aes(x = batch_nr, y = mean_best, colour = method, fill = method), data = agg) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_best - se_best, ymax = mean_best + se_best), colour = NA, alpha = 0.25)
