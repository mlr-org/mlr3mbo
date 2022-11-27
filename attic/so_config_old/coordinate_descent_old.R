#!/usr/bin/env Rscript
# chmod ug+x
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3learners)
library(mlr3pipelines)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)
library(mlr3mbo)  # @so_config
reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
library(reticulate)
library(yahpogym)
library("future")
library("future.batchtools")
library("future.apply")

source("OptimizerCoordinateDescent.R")
source("LearnerRegrRangerCustom.R")

search_space = ps(
  loop_function = p_fct(c("ego", "ego_log")),
  init = p_fct(c("random", "lhs", "sobol")),
  init_size_factor = p_int(lower = 1L, upper = 4L),
  random_interleave = p_lgl(),
  random_interleave_iter = p_fct(c("2", "4", "10"), depends = random_interleave == TRUE),

  rf_type = p_fct(c("standard", "smaclike_boot", "smaclike_no_boot", "smaclike_variance_boot")),

  acqf = p_fct(c("EI", "TTEI", "CB", "PI", "Mean")),
  lambda = p_int(lower = 1, upper = 3, depends = acqf == "CB"),
  acqopt = p_fct(c("RS_1000", "RS", "FS", "LS"))
)

instances = readRDS("instances.rds")

evaluate = function(xdt, instance) {
  library(data.table)
  library(mlr3)
  library(mlr3misc)
  library(mlr3learners)
  library(mlr3pipelines)
  library(bbotk)
  library(paradox)
  library(R6)
  library(checkmate)
  library(mlr3mbo)  # @so_config
  reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
  library(reticulate)
  library(yahpogym)

  source("LearnerRegrRangerCustom.R")

  logger = lgr::get_logger("mlr3")
  logger$set_threshold("warn")
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")

  make_optim_instance = function(instance) {
    benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
    benchmark$subset_codomain(instance$target)
    objective = benchmark$get_objective(instance$instance, multifidelity = FALSE, check_values = FALSE)
    budget = instance$budget
    optim_instance = OptimInstanceSingleCrit$new(objective, search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = budget), check_values = FALSE)
    optim_instance
  }

  optim_instance = make_optim_instance(instance)

  d = optim_instance$search_space$length
  init_design_size = d * xdt$init_size_factor
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
  } else if (xdt$rf_type == "smaclike_boot") {
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$splitrule = "extratrees"
    learner$param_set$values$num.random.splits = 1L
    learner$param_set$values$num.trees = 10
    learner$param_set$values$replace = TRUE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 1
    learner$param_set$values$mtry.ratio = 1
  } else if (xdt$rf_type == "smaclike_no_boot") {
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$splitrule = "extratrees"
    learner$param_set$values$num.random.splits = 1L
    learner$param_set$values$num.trees = 10
    learner$param_set$values$replace = FALSE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 1
    learner$param_set$values$mtry.ratio = 1
  } else if (xdt$rf_type == "smaclike_variance_boot") {
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$num.trees = 10
    learner$param_set$values$replace = TRUE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 1
    learner$param_set$values$mtry.ratio = 1
  }

  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor", multiplier = 3) %>>% learner))

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
      optimizer = OptimizerChain$new(list(opt("local_search", n_points = 100L), opt("random_search", batch_size = 1000L)), terminators = list(trm("evals", n_evals = 10010L), trm("evals", n_evals = 10000L)))
      acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 20020L))
      acq_optimizer$param_set$values$warmstart = TRUE
      acq_optimizer$param_set$values$warmstart_size = 10L
      acq_optimizer
  }

  acq_function = if (xdt$acqf == "EI") {
    AcqFunctionEI$new()
  } else if (xdt$acqf == "TTEI") {
    AcqFunctionTTEI$new(toplvl_acq_optimizer = acq_optimizer$clone(deep = TRUE))
  } else if (xdt$acqf == "CB") {
    AcqFunctionCB$new(lambda = xdt$lambda)
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

  best = optim_instance$archive$best()[[instance$target]]
  ecdf_best = instance$ecdf(best)  # evaluate the precomputed ecdf for the best value found; our target is effectively P(X <= best)
  cat("scenario:", instance$scenario, "instance:", instance$instance, "ECDF_best:", ecdf_best, "\n")
  ecdf_best
}

evaluate_all = function(xs, instances) {
  # evaluate_all will be carried out after future.batchtools scheduled a job to evaluate xs on all instances
  library(future)
  library(future.apply)
  library(future.batchtools)
  plan("batchtools_slurm", resources = list(walltime = 3600L * 7L, ncpus = 28L, memory = 1000L), template = "slurm_wyoming.tmpl")
  tmp = future_lapply(transpose_list(instances), function(instance) {
    res_instance = evaluate(xs, instance)
    #res_instance = tryCatch(evaluate(xs, instance), error = function(error_condition) 0)
  }, future.seed = TRUE)
  data.table(mean_perf = mean(unlist(tmp), na.rm = TRUE), raw_perfs = list(tmp), n_na = sum(is.na(unlist(tmp))))
}

objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    library(future)
    library(future.apply)
    library(future.batchtools)
    # FIXME: walltime can be set adaptively based on xdt
    # FIXME: we could continuously model the walltime with a surrogate and set this for each xs in xdt
    plan("batchtools_slurm", resources = list(walltime = 3600L * 7L, ncpus = 1L, memory = 1000L), template = "slurm_wyoming.tmpl")
    res = future_lapply(transpose_list(xdt), function(xs) {
      evaluate_all(xs, instances = instances)
    }, future.seed = TRUE)
    rbindlist(res)
  },
  domain = search_space,
  codomain = ps(mean_perf = p_dbl(tags = "maximize")),
  check_values = FALSE
)

cd_instance = OptimInstanceSingleCrit$new(
  objective = objective,
  search_space = search_space,
  terminator = trm("none")  # OptimizerChain currently terminates on its own
)

optimizer = OptimizerCoordinateDescent$new()
optimizer$param_set$values$max_gen = 1L

optimizer$optimize(cd_instance)
