#!/usr/bin/env Rscript
# chmod ug+x
library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3misc)
library(mlr3mbo)  # @so_config
library(bbotk)  # @localsearch
library(paradox)
library(miesmuschel) # @mlr3mbo_config
library(R6)
library(checkmate)

reticulate::use_condaenv("/home/lschnei8/.conda/envs/env", required = TRUE)
library(reticulate)
library(yahpogym)
library("future")
library("future.batchtools")
library("future.apply")

source("OptimizerCoordinateDescent.R")
source("AcqFunctionLogEI.R")
source("LearnerRegrRangerCustom.R")
source("OptimizerChain.R")

search_space = ps(
  loop_function = p_fct(c("ego", "ego_log"), default = "ego"),
  init = p_fct(c("random", "lhs", "sobol"), default = "random"),
  init_size_fraction = p_fct(c("0.0625", "0.125", "0.25"), default = "0.25"),
  random_interleave = p_lgl(default = FALSE),
  random_interleave_iter = p_fct(c("2", "5", "10"), default = "10", depends = random_interleave == TRUE),
  rf_type = p_fct(c("standard", "extratrees", "smaclike_boot", "smaclike_no_boot"), default = "standard"),
  acqf = p_fct(c("EI", "CB", "PI", "Mean"), default = "EI"),
  acqf_ei_log = p_lgl(depends = loop_function == "ego_log" && acqf == "EI", default = FALSE),
  lambda = p_fct(c("1", "3", "5"), depends = acqf == "CB", default = "1"),
  acqopt = p_fct(c("RS_1000", "RS", "FS", "LS"), default = "RS")
)

instances = setup = data.table(scenario = rep(c("lcbench", paste0("rbv2_", c("aknn", "glmnet", "ranger", "rpart", "super", "svm", "xgboost"))), each = 4L),
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

mies_average = readRDS("results_yahpo_mies_average.rds")

evaluate = function(xdt, instance) {
  id = xdt$id
  xdt$id = NULL

  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3misc)
  library(mlr3mbo)  # @so_config
  library(bbotk)  # @localsearch
  library(paradox)
  library(miesmuschel) # @mlr3mbo_config
  library(R6)
  library(checkmate)
  reticulate::use_condaenv("/home/lschnei8/.conda/envs/env", required = TRUE)
  library(reticulate)
  library(yahpogym)

  source("AcqFunctionLogEI.R")
  source("LearnerRegrRangerCustom.R")
  source("OptimizerChain.R")

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

  best = optim_instance$archive$best()[[instance$target]]
  scenario_ = instance$scenario
  instance_ = instance$instance
  target_ = paste0("mean_", instance$target)

  # assumes maximization
  k = min(mies_average[scenario == scenario_ & instance == instance_ & get(target_) >= best]$iter)  # minimum k so that best_mies[k] >= best_mbo[final]
  k = k / instance$budget  # sample efficiency compared to mies

  data.table(k = k, id = id, instance = paste0(instance$scenario, "_", instance$instance))
}

objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    xdt[, id := seq_len(.N)]
    # FIXME: walltime can be set adaptively based on xdt
    # FIXME: we could continuously model the walltime with a surrogate and set this for each xs in xdt
    # FIXME: tryCatch needed?
    res = future_mapply(FUN = evaluate, transpose_list(xdt), transpose_list(instances), SIMPLIFY = FALSE, future.seed = TRUE)
    res = rbindlist(res)
    stopifnot(nrow(res) == nrow(xdt) * nrow(instances))
    agg = res[, .(mean_k = exp(mean(log(k))), raw_k = list(k), n_na = sum(is.na(k))), by = .(id)]
    setorderv(agg, cols = "id")
    agg
  },
  domain = search_space,
  codomain = ps(mean_k = p_dbl(tags = "maximize")),
  check_values = FALSE
)

cd_instance = OptimInstanceSingleCrit$new(
  objective = objective,
  search_space = search_space,
  terminator = trm("none")  # OptimizerChain currently terminates on its own
)

optimizer = OptimizerCoordinateDescent$new()
optimizer$param_set$values$max_gen = 1L

init = data.table(loop_function = "ego", init = "random", init_size_fraction = "0.25", random_interleave = FALSE, random_interleave_iter = NA_character_, rf_type = "standard", acqf = "EI", acqf_ei_log = NA, lambda = NA_character_, acqopt = "RS")
set.seed(2906)
plan("batchtools_slurm", resources = list(walltime = 3600L * 12L, ncpus = 1L, memory = 4000L), template = "slurm_wyoming.tmpl")
cd_instance$eval_batch(init)
optimizer$optimize(cd_instance)

