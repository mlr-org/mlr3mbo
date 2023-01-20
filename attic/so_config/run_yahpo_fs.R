library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(bbotk)  # @localsearch
library(paradox)
library(R6)
library(checkmate)

reticulate::use_condaenv("/home/lschnei8/.conda/envs/env", required = TRUE)
library(reticulate)
yahpo_gym = import("yahpo_gym")

packages = c("data.table", "mlr3", "mlr3misc", "bbotk", "paradox", "R6", "checkmate")

#RhpcBLASctl::blas_set_num_threads(1L)
#RhpcBLASctl::omp_set_num_threads(1L)

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_yahpo_fs", packages = packages)
#reg = makeExperimentRegistry(file.dir = NA, conf.file = NA, packages = packages)  # interactive session
saveRegistry(reg)

fs_wrapper = function(job, data, instance, ...) {
  # fs is our baseline with 3000 x more budget
  reticulate::use_condaenv("/home/lschnei8/.conda/envs/env", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  make_optim_instance = function(instance) {
    benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
    benchmark$subset_codomain(instance$target)
    objective = benchmark$get_objective(instance$instance, multifidelity = FALSE, check_values = FALSE)
    budget = instance$budget
    optim_instance = OptimInstanceSingleCrit$new(objective, search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = budget), check_values = FALSE)
    optim_instance
  }

  n_evals = instance$budget
  batch_size = 10000L
  maxit = ceiling((n_evals / (batch_size)))

  optimizer = opt("focus_search", n_points = batch_size, maxit = maxit)

  optim_instance = make_optim_instance(instance)
  optimizer$optimize(optim_instance)
  optim_instance
}


# add algorithms
addAlgorithm("fs", fun = fs_wrapper)

setup = data.table(scenario = rep(c("lcbench", paste0("rbv2_", c("aknn", "glmnet", "ranger", "rpart", "super", "svm", "xgboost"))), each = 4L),
                   instance = c("167185", "167152", "168910", "189908",
                                "40499", "1476", "6", "12",
                                "40979", "1501", "40966", "1478",
                                "12", "458", "1510", "1515",
                                "1478", "40979", "12", "28",
                                "41164", "37", "1515", "1510",
                                "1478", "1501", "40499", "40979",
                                "40984", "40979", "40966", "28"),
                   target = rep(c("val_accuracy", "acc"), c(4L, 28L)),
                   budget = rep(c(126L, 118L, 90L, 134L, 110L, 267L, 118L, 170L), each = 4L))
setup[, budget := budget * 3000L]

setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$target)
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, target = setup[i, ]$target, budget = setup[i, ]$budget, on_integer_scale = setup[i, ]$on_integer_scale))
  setNames(list(setup[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add jobs for optimizers
optimizers = data.table(algorithm = c("fs"))

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 30L
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

# rbv2_super 801000 budget needs ~ 15 minutes so 20 chunks results in roughly 5 hours
jobs = findJobs()
jobs[, chunk := batchtools::chunk(job.id, chunk.size = 20L)]
resources.default = list(walltime = 3600 * 6L, memory = 16000, ntasks = 1L, ncpus = 2L, nodes = 1L, clusters = "beartooth", max.concurrent.jobs = 9999L)
submitJobs(jobs, resources = resources.default)

done = findDone()
results = reduceResultsList(done, function(x, job) {
  x = x$archive$data
  pars = job$pars
  target_var = pars$prob.pars$target
  tmp = x[, eval(target_var), with = FALSE]
  colnames(tmp) = "target"
  tmp[, best := cummax(target)]
  tmp[, method := pars$algo.pars$algorithm]
  tmp[, scenario := pars$prob.pars$scenario]
  tmp[, instance := pars$prob.pars$instance]
  tmp[, repl := job$repl]
  tmp[, iter := seq_len(.N)]
  tmp
})
results = rbindlist(results, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/results_yahpo_fs.rds")

mean_results = results[, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(scenario, instance, iter)]
saveRDS(mean_results, "/gscratch/lschnei8/results_yahpo_fs_average.rds")

library(ggplot2)
library(gridExtra)

lm_data = copy(mean_results)
lm_data[, problem := paste0(scenario, "_", instance)]
models = map_dtr(unique(lm_data$problem), function(problem_) {
  tmp = lm_data[problem == problem_]
  values = tail(unique(tmp$mean_best), 2L)
  dat = map_dtr(values, function(value) {
    tmp[mean_best == value][.N, ]
  })
  model = lm(mean_best ~ iter, data = dat)
  coefs = coef(model)
  if (coefs[2L] < .Machine$double.eps) {
    stop("Almost constant linear model")
  }
  max_mean_best = max(tmp$mean_best)
  max_iter = max(tmp$iter)
  estimate_iter = function(mean_best, correct = TRUE) {
    stopifnot(mean_best >= max_mean_best)
    iter = as.integer(ceiling((mean_best - coefs[1L]) / coefs[2L]))
    if (correct) {
      if (mean_best > max_mean_best && iter <= max_iter) {
        iter = max_iter + 1L
      }
    }
    iter
  }
  env = new.env()
  environment(estimate_iter) = env
  assign("max_mean_best", value = max_mean_best, envir = env)
  assign("max_iter", value = max_iter, envir = env)
  if (estimate_iter(1.00001 * max(tmp$mean_best), correct = FALSE) < max(tmp$iter)) {
    # marginal improvements should require more iter than max iter
    stop("Model does not interpolate latest iter well.")
  }
  tmp_p = data.table(iter = ((NROW(tmp) - 1L):ceiling(1.5 * NROW(tmp))))
  p = predict(model, tmp_p)
  tmp_p[, mean_best := p]
  tmp_p[, method := "interpolation"]
  tmp_plot = copy(tmp)[, c("iter", "mean_best")]
  tmp_plot[, method := "real"]
  tmp_plot = rbind(tmp_plot, tmp_p)
  g = ggplot(aes(x = iter, y = mean_best, colour = method), data = tmp_plot[ceiling(0.9 * NROW(tmp)):.N , ]) +
    scale_y_log10() +
    geom_step(direction = "vh") +
    geom_hline(yintercept = max(tmp$mean_best), linetype = 2L) +
    labs(title = problem_) +
    theme_minimal() +
    theme(legend.position = "bottom")
  info = strsplit(problem_, "_")[[1L]]
  if (length(info) == 3L) {
    info = c(paste0(info[1L], "_", info[2L]), info[3L])  # rbv2_
  }
  list(model = list(estimate_iter), plot = list(g), scenario = info[1L], instance = info[2L])
})

g = do.call("grid.arrange", c(models$plot, ncol = 8L))

ggsave("fs_extrapolation.png", plot = g, width = 32, height = 12)

saveRDS(models[, - "plot"], "fs_extrapolation.rds")

