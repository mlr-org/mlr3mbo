library(batchtools)
library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3misc)
library(mlr3mbo)  # @so_config
library(bbotk)  # @localsearch
library(paradox)
library(R6)
library(checkmate)

reticulate::use_condaenv("/home/lschnei8/.conda/envs/env", required = TRUE)
library(reticulate)
yahpo_gym = import("yahpo_gym")

packages = c("data.table", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3misc", "mlr3mbo", "bbotk", "paradox", "R6", "checkmate")

#RhpcBLASctl::blas_set_num_threads(1L)
#RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr(c("helpers.R", "AcqFunctionLogEI.R", "LearnerRegrRangerCustom.R", "OptimizerChain.R"), function(x) file.path(experiments_dir, x))
for (sf in source_files) {
  source(sf)
}

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_mlr3mbo_so_config", packages = packages, source = source_files)
#reg = makeExperimentRegistry(file.dir = NA, conf.file = NA, packages = packages, source = source_files)  # interactive session
saveRegistry(reg)

mlr3mbo_wrapper = function(job, data, instance, ...) {
  reticulate::use_condaenv("/home/lschnei8/.conda/envs/env", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  xdt = data.table(loop_function = "ego_log", init = "random", init_size_fraction = "0.25", random_interleave = TRUE, random_interleave_iter = "10", rf_type = "smaclike_no_boot", acqf = "CB", acqf_ei_log = NA, lambda = "1", acqopt = "LS")

  init_design_size = ceiling(as.numeric(xdt$init_size_fraction) * optim_instance$terminator$param_set$values$n_evals)
  init_design = if (xdt$init == "random") {
    generate_design_random(optim_instance$search_space, n = init_design_size)$data
  } else if (xdt$init == "lhs") {
    generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  } else if (xdt$init == "sobol") {
    generate_design_sobol(optim_instance$search_space, n = init_design_size)$data
  }

  optim_instance$eval_batch(init_design)

  random_interleave_iter = if(xdt$random_interleave) as.numeric(xdt$random_interleave_iter) else 0L

  learner = LearnerRegrRangerCustom$new()
  learner$predict_type = "se"
  learner$param_set$values$keep.inbag = TRUE

  if (xdt$rf_type == "standard") {
    learner$param_set$values$se.method = "jack"
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$num.trees = 1000L
  } else if (xdt$rf_type == "extratrees") {
    learner$param_set$values$se.method = "jack"
    learner$param_set$values$splitrule = "extratrees"
    learner$param_set$values$num.random.splits = 1L
    learner$param_set$values$num.trees = 1000L
  } else if (xdt$rf_type == "smaclike_boot") {
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$splitrule = "extratrees"
    learner$param_set$values$num.random.splits = 1L
    learner$param_set$values$num.trees = 10L
    learner$param_set$values$replace = TRUE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 1
    learner$param_set$values$mtry.ratio = 1
  } else if (xdt$rf_type == "smaclike_no_boot") {
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$splitrule = "extratrees"
    learner$param_set$values$num.random.splits = 1L
    learner$param_set$values$num.trees = 10L
    learner$param_set$values$replace = FALSE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 1
    learner$param_set$values$mtry.ratio = 1
  }

  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>%
                                                    po("imputeoor", multiplier = 3, affect_columns = selector_type(c("integer", "numeric", "character", "factor", "ordered"))) %>>%
                                                    po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
                                                    learner))

  acq_optimizer = if (xdt$acqopt == "RS_1000") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
  } else if (xdt$acqopt == "RS") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 20000L))
  } else if (xdt$acqopt == "FS") {
      n_repeats = 2L
      maxit = 9L
      batch_size = ceiling((20000L / n_repeats) / (1 + maxit))  # 1000L
      AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 20000L))
  } else if (xdt$acqopt == "LS") {
      optimizer = OptimizerChain$new(list(opt("local_search", n_points = 100L), opt("random_search", batch_size = 1000L)), terminators = list(trm("evals", n_evals = 10000L), trm("evals", n_evals = 10000L)))
      acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 20000L))
      acq_optimizer$param_set$values$warmstart = TRUE
      acq_optimizer$param_set$values$warmstart_size = "all"
      acq_optimizer
  }

  acq_function = if (xdt$acqf == "EI") {
    if (isTRUE(xdt$acqf_ei_log)) {
      AcqFunctionLogEI$new()
    } else {
      AcqFunctionEI$new()
    }
  } else if (xdt$acqf == "CB") {
    AcqFunctionCB$new(lambda = as.numeric(xdt$lambda))
  } else if (xdt$acqf == "PI") {
    AcqFunctionPI$new()
  } else if (xdt$acqf == "Mean") {
    AcqFunctionMean$new()
  }

  if (xdt$loop_function == "ego") {
    bayesopt_ego(optim_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)
  } else if (xdt$loop_function == "ego_log") {
    bayesopt_ego_log(optim_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)
  }

  optim_instance
}

# add algorithms
addAlgorithm("mlr3mbo", fun = mlr3mbo_wrapper)

# setup scenarios and instances
get_nb301_setup = function(budget_factor = 40L) {
  scenario = "nb301"
  bench = yahpo_gym$benchmark_set$BenchmarkSet(scenario, instance = "CIFAR10")
  fidelity_space = bench$get_fidelity_space()
  fidelity_param_id = fidelity_space$get_hyperparameter_names()[1]
  min_budget = fidelity_space$get_hyperparameter(fidelity_param_id)$lower
  max_budget = fidelity_space$get_hyperparameter(fidelity_param_id)$upper
  ndim = length(bench$config_space$get_hyperparameter_names()) - 1L  # NOTE: instance is not part of

  instances = "CIFAR10"
  target = "val_accuracy"
  budget = ceiling(20L * max_budget + sqrt(ndim) * max_budget * budget_factor)
  on_integer_scale = TRUE
  minimize = bench$config$config$y_minimize[match(target, bench$config$config$y_names)]
  setup = setDT(expand.grid(scenario = scenario, instance = instances, target = target, ndim = ndim, max_budget = max_budget, budget = budget, on_integer_scale = on_integer_scale, minimize = minimize, stringsAsFactors = FALSE))
  setup
}

get_lcbench_setup = function(budget_factor = 40L) {
  scenario = "lcbench"
  bench = yahpo_gym$benchmark_set$BenchmarkSet(scenario, instance = "167168")
  fidelity_space = bench$get_fidelity_space()
  fidelity_param_id = fidelity_space$get_hyperparameter_names()[1]
  min_budget = fidelity_space$get_hyperparameter(fidelity_param_id)$lower
  max_budget = fidelity_space$get_hyperparameter(fidelity_param_id)$upper
  ndim = length(bench$config_space$get_hyperparameter_names()) - 2L

  instances = c("167168", "189873", "189906")
  target = "val_accuracy"
  budget = ceiling(20L * max_budget + sqrt(ndim) * max_budget * budget_factor)
  on_integer_scale = TRUE
  minimize = bench$config$config$y_minimize[match(target, bench$config$config$y_names)]
  setup = setDT(expand.grid(scenario = scenario, instance = instances, target = target, ndim = ndim, max_budget = max_budget, budget = budget, on_integer_scale = on_integer_scale, minimize = minimize, stringsAsFactors = FALSE))
  setup
}

get_rbv2_setup = function(budget_factor = 40L) {
  setup = map_dtr(c("rbv2_glmnet", "rbv2_rpart", "rbv2_ranger", "rbv2_xgboost", "rbv2_super"), function(scenario) {
    bench = yahpo_gym$benchmark_set$BenchmarkSet(scenario, instance = "1040")
    fidelity_space = bench$get_fidelity_space()
    fidelity_param_id = "trainsize"
    min_budget = fidelity_space$get_hyperparameter(fidelity_param_id)$lower
    max_budget = fidelity_space$get_hyperparameter(fidelity_param_id)$upper
    ndim = length(bench$config_space$get_hyperparameter_names()) - 3L  # repl and trainsize and instance

    instances = switch(scenario, rbv2_glmnet = c("375", "458"), rbv2_rpart = c("14", "40499"), rbv2_ranger = c("16", "42"), rbv2_xgboost = c("12", "1501", "16", "40499"), rbv2_super = c("1053", "1457", "1063", "1479", "15", "1468"))
    target = "acc"
    budget = ceiling(20L * max_budget + sqrt(ndim) * max_budget * budget_factor)
    on_integer_scale = FALSE
    minimize = bench$config$config$y_minimize[match(target, bench$config$config$y_names)]
    setup = setDT(expand.grid(scenario = scenario, instance = instances, target = target, ndim = ndim, max_budget = max_budget, budget = budget, on_integer_scale = on_integer_scale, minimize = minimize, stringsAsFactors = FALSE))
  })
}

setup = rbind(get_nb301_setup(), get_lcbench_setup(), get_rbv2_setup())

setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$target)
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, target = setup[i, ]$target, ndim = setup[i, ]$ndim, max_budget = setup[i, ]$max_budget, budget = setup[i, ]$budget, on_integer_scale = setup[i, ]$on_integer_scale, minimize = setup[i, ]$minimize))
  setNames(list(setup[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add jobs for optimizers
optimizers = data.table(algorithm = "mlr3mbo")

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 30L
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

jobs = getJobTable()
resources.default = list(walltime = 3600 * 8L, memory = 2048L, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth", max.concurrent.jobs = 9999L)
submitJobs(jobs, resources = resources.default)

done = findDone()
results = reduceResultsList(done, function(x, job) {
  x = x$archive$data
  budget_var = if (job$instance$scenario %in% c("lcbench", "nb301")) "epoch" else "trainsize"
  target_var = job$instance$target
  if (!job$instance$minimize) {
    x[, (target_var) := - get(target_var)]
  }
  pars = job$pars
  tmp = x[, target_var, with = FALSE]
  tmp[, (budget_var) := job$instance$max_budget]
  tmp[, method := pars$algo.pars$algorithm]
  tmp[, scenario := pars$prob.pars$scenario]
  tmp[, instance := pars$prob.pars$instance]
  tmp[, repl := job$repl]
  tmp[, iter := seq_len(.N)]
  colnames(tmp) = c("target", "budget", "method", "scenario", "instance", "repl", "iter")
  tmp
})
results = rbindlist(results, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/results_yahpo_mlr3mbo.rds")

