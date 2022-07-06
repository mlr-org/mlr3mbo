#!/usr/bin/env Rscript
# chmod ug+x
library(argparse)
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3learners)
library(mlr3pipelines)
library(bbotk)  # @focussearch
library(paradox)
library(mlr3mbo)  # @so_config
reticulate::use_virtualenv("/home/lschnei8/yahpo_gym/experiments/mf_env/", required = TRUE)
library(reticulate)
library(yahpogym)
library("future")
library("future.apply")

# FIXME: test stability stuff?

parser = ArgumentParser()
parser$add_argument("-r", "--run", type = "integer", default = 1, help = "Id of run, should be within 1-5")
args = parser$parse_args()
run_id = args$run
stopifnot(run_id %in% 1:5)
cat("run_id:", run_id, "\n")

seeds = c(2409, 2906, 0905, 1234, 1010)

set.seed(seeds[run_id])

search_space = ps(
  init = p_fct(c("random", "lhs")),
  init_size_factor = p_int(lower = 1L, upper = 6L),
  random_interleave = p_lgl(),
  random_interleave_iter = p_int(lower = 2L, upper = 10L, depends = random_interleave == TRUE),
  #surrogate = p_fct(c("GP", "RF")),  # FIXME: BANANAS NN ensemble
  #splitrule = p_fct(c("variance", "extratrees"), depends = surrogate == "RF"),
  #num.random.splits = p_int(lower = 1L, upper = 10L, depends = surrogate == "RF" && splitrule == "extratrees"),
  num.trees = p_int(lower = 100L, upper = 2000L),
  splitrule = p_fct(c("variance", "extratrees")),
  num.random.splits = p_int(lower = 1L, upper = 10L, depends = splitrule == "extratrees"),
  acqf = p_fct(c("EI", "CB", "PI")),
  lambda = p_dbl(lower = 0, upper = 3L, depends = acqf == "CB"),
  acqopt_iter_factor = p_int(lower = 1L, upper = 20L),  # lowest acqopt_iter is 1000 * 1
  acqopt = p_fct(c("RS", "FS")),  # FIXME: miesmuschel
  fs_behavior = p_fct(c("global", "local"), depends = acqopt == "FS")
)

# budget = ceiling(20 + sqrt(d) * 40)
instances = readRDS("instances.rds")
#instances = data.table(scenario = rep(paste0("rbv2_", c("aknn", "glmnet", "ranger", "rpart", "super", "svm", "xgboost")), each = 5L),
#                       instance = c("40499", "1476", "6", "12", "41150",
#                                    "40979", "1501", "40966", "1478", "40984",
#                                    "12", "458", "1510", "1515", "307",
#                                    "1478", "40979", "12", "28", "1501",
#                                    "41164", "37", "1515", "1510", "42",
#                                    "1478", "1501", "40499", "40979", "300",
#                                    "40984", "40979", "40966", "28", "22"),
#                       target = "acc",
#                       budget = rep(c(118, 90, 134, 110, 267, 118, 170), each = 5L))

make_optim_instance = function(instance) {
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE, check_values = FALSE)
  budget = instance$budget
  optim_instance = OptimInstanceSingleCrit$new(objective, search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = budget), check_values = FALSE)
  optim_instance
}

evaluate = function(xdt, instance) {
  logger = lgr::get_logger("mlr3")
  logger$set_threshold("warn")
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")

  optim_instance = make_optim_instance(instance)

  d = optim_instance$search_space$length
  init_design_size = d * xdt$init_size_factor
  init_design = if (xdt$init == "random") generate_design_random(optim_instance$search_space, n = init_design_size)$data else if (xdt$init == "lhs") generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  optim_instance$eval_batch(init_design)
  
  random_interleave_iter = if(xdt$random_interleave) xdt$random_interleave_iter else 0L
  
  #surrogate = if(xdt$surrogate == "GP") {
  #  learner = lrn("regr.km", covtype = "matern3_2", optim.method = "gen", nugget.stability = 10^-8)
  #  SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% po("encodeimpact", affect_columns = selector_type(c("logical", "character", "factor", "ordered"))) %>>% learner))
  #} else if (xdt$surrogate == "RF") {
  #  learner = if (xdt$splitrule == "extratrees") {
  #    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule, num.random.splits = xdt$num.random.splits)
  #  } else if (xdt$splitrule == "variance") {
  #    lrn("regr.ranger", num.trees = xdt$num.trees, keep.inbag = TRUE, splitrule = xdt$splitrule)
  #  }
  #  SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))
  #}

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
  
  best = optim_instance$archive$best()[[instance$target]]
  # (best - instance$mean) / instance$sd  # normalize best w.r.t min_max.R empirical mean and sd
  ecdf_best = instance$ecdf(best)  # evaluate the precomputed ecdf for the best value found; our target is effectively P(X <= best)
  cat("scenario:", instance$scenario, "instance:", instance$instance, "ECDF_best:", ecdf_best, "\n")
  ecdf_best
}

objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    saveRDS(ac_instance, paste0("ac_instance_", run_id, ".rds"))
    map_dtr(seq_len(nrow(xdt)), function(i) {
      plan("multicore")
      tmp = future_lapply(transpose_list(instances), function(instance) {
        res_instance = tryCatch(evaluate(xdt[i, ], instance), error = function(error_condition) NA_real_)
      }, future.seed = TRUE)
      data.table(mean_perf = mean(unlist(tmp), na.rm = TRUE), raw_perfs = list(tmp), n_na = sum(is.na(unlist(tmp))))
    })
  },
  domain = search_space,
  codomain = ps(mean_perf = p_dbl(tags = "maximize"))
)

ac_instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 230L)  # 30 init design + 200
)

surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% lrn("regr.ranger", num.trees = 2000L, keep.inbag = TRUE)))
acq_function = AcqFunctionEI$new()
acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 10000L))
design = generate_design_lhs(ac_instance$search_space, n = 30L)$data
ac_instance$eval_batch(design)
saveRDS(ac_instance, paste0("ac_instance_", run_id, ".rds"))
bayesopt_ego(ac_instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = 5L)
saveRDS(ac_instance, paste0("ac_instance_", run_id, ".rds"))

