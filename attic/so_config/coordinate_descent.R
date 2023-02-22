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
library(R6)
library(checkmate)

# FIXME: error handling
#        how to continue after one job is finished (i.e. from an intermediate gen) save seed?

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
  init_size_fraction = p_fct(c("0.05", "0.10", "0.25"), default = "0.25"),
  random_interleave = p_lgl(default = FALSE),
  random_interleave_iter = p_fct(c("2", "5", "10"), depends = random_interleave == TRUE, default = "10"),
  rf_type = p_fct(c("standard", "extratrees", "smaclike_boot", "smaclike_no_boot"), default = "standard"),
  acqf = p_fct(c("EI", "CB", "PI", "Mean"), default = "EI"),
  acqf_ei_log = p_lgl(depends = loop_function == "ego_log" && acqf == "EI", default = FALSE),
  lambda = p_fct(c("1", "3", "10"), depends = acqf == "CB", default = "1"),
  acqopt = p_fct(c("RS_1000", "RS", "FS", "LS"), default = "RS_1000")
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
instances[, problem := paste0(scenario, "_", instance)]
setorderv(instances, col = "budget", order = -1L)

fs_average = readRDS("/gscratch/lschnei8/results_yahpo_fs_average.rds")
fs_extrapolation = readRDS("/gscratch/lschnei8/fs_extrapolation.rds")

get_k = function(best, scenario_, instance_, budget_) {
  # assumes maximization
  if (best > max(fs_average[scenario == scenario_ & instance == instance_][["mean_best"]])) {
    extrapolate = TRUE
    k = fs_extrapolation[scenario == scenario_ & instance == instance_][["model"]][[1L]](best)
  } else {
    extrapolate = FALSE
    k = min(fs_average[scenario == scenario_ & instance == instance_ & mean_best >= best]$iter)  # min k so that mean_best_fs[k] >= best_mbo[final]
  }
  k = k / budget_  # sample efficiency compared to fs
  attr(k, "extrapolate") = extrapolate
  k
}

evaluate = function(xdt, instance) {
  id = xdt$id
  repl = xdt$repl
  xdt$id = NULL
  xdt$repl = NULL

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

  surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>%
                                                    po("imputeoor", multiplier = 3, affect_columns = selector_type(c("integer", "numeric", "character", "factor", "ordered"))) %>>%
                                                    po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
                                                    learner))
  surrogate$param_set$values$catch_errors = FALSE

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
  acq_optimizer$param_set$values$catch_errors = FALSE

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

  data.table(best = best, scenario = instance$scenario, instance = instance$instance, target = instance$target, id = id, repl = repl)
}

objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    n_repl = 5L
    xdt[, id := seq_len(.N)]
    xdt_tmp = map_dtr(seq_len(nrow(instances)), function(i) copy(xdt))
    setorderv(xdt_tmp, col = "id")
    xdt_tmp = map_dtr(seq_len(n_repl), function(i) {
      tmp = copy(xdt_tmp)
      tmp[, repl := i]
      tmp
    })
    instances_tmp = map_dtr(seq_len(nrow(xdt) * n_repl), function(i) copy(instances))
    current_seed = .Random.seed
    res = tryCatch(future_mapply(FUN = evaluate, transpose_list(xdt_tmp), transpose_list(instances_tmp), SIMPLIFY = FALSE, future.seed = TRUE, future.scheduling = FALSE), error = function(ec) ec)
    if (inherits(res, "error")) {
      browser()  # last manual fallback resort to get things working again
      # cleanup future stuff
      # reset the current seed and continue the eval
      # .Random.seed = current_seed
      # res = future_mapply(FUN = evaluate, transpose_list(xdt_tmp), transpose_list(instances_tmp), SIMPLIFY = FALSE, future.seed = TRUE, future.scheduling = FALSE)
    }
    res = rbindlist(res)
    setorderv(res, col = "instance")
    setorderv(res, col = "id")
    setorderv(res, col = "repl")
    res[, problem := paste0(scenario, "_", instance)]

    # average best over replications and determine ks
    agg = res[, .(mean_best = mean(best), raw_best = list(best), n_na = sum(is.na(best)), n = .N), by = .(id, problem, scenario, instance)]
    ks = map_dbl(seq_len(nrow(agg)), function(i) {
      if (agg[i, ][["n"]] < n_repl) {
        0
      } else {
        tryCatch(
          get_k(agg[i, ][["mean_best"]],
                scenario_ = agg[i, ][["scenario"]],
                instance_ = agg[i, ][["instance"]],
                budget_ = instances[problem == agg[i, ][["problem"]]][["budget"]]),
          error = function(ec) 0
        )
      }
    })
    agg[, k := ks]

    # average k over instances and determine mean_k
    agg_k = agg[, .(mean_k = exp(mean(log(k))), raw_k = list(k), n_na = sum(is.na(k)), n = .N, raw_mean_best = list(mean_best)), by = .(id)]
    agg_k[n < nrow(instances), mean_k := 0]
    agg_k
  },
  domain = search_space,
  codomain = ps(mean_k = p_dbl(tags = "maximize")),
  check_values = FALSE
)

cd_instance = OptimInstanceSingleCrit$new(
  objective = objective,
  search_space = search_space,
  terminator = trm("none")  # OptimizerCoordinateDescent currently terminates on its own
)

optimizer = OptimizerCoordinateDescent$new()
optimizer$param_set$values$max_gen = 5L

init = data.table(loop_function = "ego", init = "random", init_size_fraction = "0.25", random_interleave = FALSE, random_interleave_iter = NA_character_, rf_type = "standard", acqf = "EI", acqf_ei_log = NA, lambda = NA_character_, acqopt = "RS_1000")
options(future.cache.path = "/home/lschnei8/mlr3mbo_config/future")
set.seed(2906, kind = "L'Ecuyer-CMRG")
# currently we evaluate at most 4 * 32 * 5 jobs in parallel so 650 workers is enough
plan("batchtools_slurm", template = "slurm_wyoming_cd.tmpl", resources = list(walltime = 3600L * 9L, ncpus = 1L, memory = 4000L), workers = 650L)
cd_instance$eval_batch(init)
optimizer$optimize(cd_instance)

