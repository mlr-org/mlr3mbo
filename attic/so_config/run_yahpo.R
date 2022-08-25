library(batchtools)
library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3misc)
library(mlr3mbo)  # @so_config
library(bbotk)
library(paradox)
reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
library(reticulate)
yahpo_gym = import("yahpo_gym")

packages = c("data.table", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3misc", "mlr3mbo", "bbotk", "paradox", "mlrintermbo")

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr("helpers.R", function(x) file.path(experiments_dir, x))
for (sf in source_files) {
  source(sf)
}

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_mlr3mbo_so_config", packages = packages, source = source_files)
#reg = makeExperimentRegistry(file.dir = NA, conf.file = NA, packages = packages, source = source_files)  # interactive session
saveRegistry(reg)

# FIXME: also compare jack vs. infjack?
mlr3mbo_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  xdt = list(init = "lhs", init_size_factor = 4L, random_interleave = FALSE, num.trees = 250L, splitrule = "extratrees", num.random.splits = 8, acqf = "CB", lambda = 2.8, acqopt_iter_factor = 6L, acqopt = "FS", fs_behavior = "global")

  d = optim_instance$search_space$length
  init_design_size = d * xdt$init_size_factor
  init_design = if (xdt$init == "random") generate_design_random(optim_instance$search_space, n = init_design_size)$data else if (xdt$init == "lhs") generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  optim_instance$eval_batch(init_design)
  
  random_interleave_iter = if(xdt$random_interleave) xdt$random_interleave_iter else 0L
  
  learner = if (xdt$splitrule == "extratrees") {
    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule, num.random.splits = xdt$num.random.splits)
  } else if (xdt$splitrule == "variance") {
    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule)
  }
  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))

  acq_function = if (xdt$acqf == "EI") {
    AcqFunctionEI$new()
  } else if (xdt$acqf == "CB") {
    AcqFunctionCB$new(lambda = xdt$lambda)
  } else if (xdt$acqf == "PI") {
    AcqFunctionPI$new()
  }
  
  acq_budget = 1000 * xdt$acqopt_iter_factor
  
  acq_optimizer = if (xdt$acqopt == "RS") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = acq_budget))
  } else if (xdt$acqopt == "FS") {
    if (xdt$fs_behavior == "global") {
      n_repeats = 10L
      maxit = 2L
      batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    } else if (xdt$fs_behavior == "local") {
      n_repeats = 2L
      maxit = 10L
      batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    }
    AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = acq_budget))
  }
  
  bayesopt_ego(optim_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)
  optim_instance
}

mlrintermbo_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)
  optimizer = opt("intermbo", on.surrogate.error = "stop")
  learner = lrn("regr.ranger", se.method = "jack", keep.inbag = TRUE)
  learner$predict_type = "se"
  learner = GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner)
  learner$predict_type = "se"
  #learner = mlr::makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE)
  #learner = mlr::makeImputeWrapper(learner, classes = list(numeric = mlr::imputeMax(2), factor = mlr::imputeConstant("__miss__"), logical = mlr::imputeUniform()))
  #learner = mlr::setPredictType(learner, "se")
  #optimizer$param_set$values$surrogate.learner = learner
  optimizer$optimize(optim_instance)
  optim_instance
}

mlr3mbo_default_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  xdt = list(init = "random", init_size_factor = 4L, random_interleave = FALSE, num.trees = 500L, splitrule = "variance", num.random.splits = NA_integer_, acqf = "EI", lambda = NA_real_, acqopt_iter_factor = 10L, acqopt = "RS", fs_behavior = NA_character_)

  d = optim_instance$search_space$length
  init_design_size = d * xdt$init_size_factor
  init_design = if (xdt$init == "random") generate_design_random(optim_instance$search_space, n = init_design_size)$data else if (xdt$init == "lhs") generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  optim_instance$eval_batch(init_design)
  
  random_interleave_iter = if(xdt$random_interleave) xdt$random_interleave_iter else 0L
  
  learner = if (xdt$splitrule == "extratrees") {
    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule, num.random.splits = xdt$num.random.splits)
  } else if (xdt$splitrule == "variance") {
    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule)
  }
  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))

  acq_function = if (xdt$acqf == "EI") {
    AcqFunctionEI$new()
  } else if (xdt$acqf == "CB") {
    AcqFunctionCB$new(lambda = xdt$lambda)
  } else if (xdt$acqf == "PI") {
    AcqFunctionPI$new()
  }
  
  acq_budget = 1000 * xdt$acqopt_iter_factor
  
  acq_optimizer = if (xdt$acqopt == "RS") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = acq_budget))
  } else if (xdt$acqopt == "FS") {
    if (xdt$fs_behavior == "global") {
      n_repeats = 10L
      maxit = 2L
      batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    } else if (xdt$fs_behavior == "local") {
      n_repeats = 2L
      maxit = 10L
      batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    }
    AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = acq_budget))
  }
  
  bayesopt_ego(optim_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)
  optim_instance
}

mlr3mbo_wrapper_custom = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  xdt = list(init = "lhs", init_size_factor = 1L, random_interleave = FALSE, num.trees = 250L, splitrule = "extratrees", num.random.splits = 10, acqf = "CB", lambda = 3, acqopt_iter_factor = 30L, acqopt = "FS", fs_behavior = "global")

  d = optim_instance$search_space$length
  init_design_size = d * xdt$init_size_factor
  init_design = if (xdt$init == "random") generate_design_random(optim_instance$search_space, n = init_design_size)$data else if (xdt$init == "lhs") generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  optim_instance$eval_batch(init_design)
  
  random_interleave_iter = if(xdt$random_interleave) xdt$random_interleave_iter else 0L
  
  learner = if (xdt$splitrule == "extratrees") {
    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule, num.random.splits = xdt$num.random.splits)
  } else if (xdt$splitrule == "variance") {
    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule)
  }
  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))

  acq_function = if (xdt$acqf == "EI") {
    AcqFunctionEI$new()
  } else if (xdt$acqf == "CB") {
    AcqFunctionCB$new(lambda = xdt$lambda)
  } else if (xdt$acqf == "PI") {
    AcqFunctionPI$new()
  }
  
  acq_budget = 1000 * xdt$acqopt_iter_factor
  
  acq_optimizer = if (xdt$acqopt == "RS") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = acq_budget))
  } else if (xdt$acqopt == "FS") {
    if (xdt$fs_behavior == "global") {
      n_repeats = 5L
      maxit = 5L
      batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    } else if (xdt$fs_behavior == "local") {
      n_repeats = 2L
      maxit = 10L
      batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    }
    AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = acq_budget))
  }
  
  bayesopt_ego(optim_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)
  optim_instance
}

mlr3mbo_wrapper_new_rf = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  d = optim_instance$search_space$length
  init_design_size = max(c(3L, d * 1L))
  init_design = generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  optim_instance$eval_batch(init_design)
  
  random_interleave_iter = 0L
  
  learner = lrn("regr.ranger_custom")
  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))

  acq_function = AcqFunctionEI$new()
  
  acq_budget = 20000L
  
  acq_optimizer = {
    n_repeats = 2L
    maxit = 9L
    batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
    AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = acq_budget))
  }
  
  bayesopt_ego(optim_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)
  optim_instance
}

# add algorithms
addAlgorithm("mlr3mbo", fun = mlr3mbo_wrapper)
addAlgorithm("mlrintermbo", fun = mlrintermbo_wrapper)
addAlgorithm("mlr3mbo_default", fun = mlr3mbo_default_wrapper)
addAlgorithm("mlr3mbo_custom", fun = mlr3mbo_wrapper_custom)
addAlgorithm("mlr3mbo_new_rf", fun = mlr3mbo_wrapper_new_rf)

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
optimizers = data.table(algorithm = c("mlr3mbo", "mlrintermbo", "mlr3mbo_default", "mlr3mbo_custom", "mlr3mbo_new_rf"))

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 30L
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

jobs = findJobs()
resources.default = list(walltime = 3600 * 5L, memory = 2048L, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton", max.concurrent.jobs = 9999L)
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
saveRDS(results, "results_yahpo_own.rds")

