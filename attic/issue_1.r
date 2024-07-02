devtools::load_all(".")
library(batchtools)
library(mlr3misc)
library(data.table)
library(paradox)
library(bbotk)
library(reticulate)
library(yahpogym)
library(batchtools)
library(mlr3misc)
library(data.table)
library(paradox)
library(bbotk)
library(mlr3learners)
library(mlr3mbo)
library(mlr3pipelines)
library(reticulate)
library(yahpogym)

use_condaenv("yahpo_gym", required=TRUE)
yahpo_gym = import("yahpo_gym")

log_scale = TRUE
init = "random"
init_size_fraction = 0.25
random_interleave_iter = 0
rf_type = "standard"
acqf = "EI"
acqopt = "RS_1000"
lambda = NA_real_
id = 1L
config_hash = "88169553-ad40-4225-98ef-0579f4207f43"

random_interleave_iter = as.numeric(random_interleave_iter)
init_size_fraction = as.numeric(init_size_fraction)
lambda = as.numeric(lambda)

use_condaenv("yahpo_gym", required = TRUE)
yahpo_gym = import("yahpo_gym")

benchmark = BenchmarkSet$new("rbv2_svm")
benchmark$subset_codomain("acc")
objective = benchmark$get_objective("40981", multifidelity = FALSE)

optim_instance = oi(
  objective,
  search_space = benchmark$get_search_space(drop_fidelity_params = TRUE),
  terminator = trm("evals", n_evals = 200),
  check_values = FALSE)

init_design_size = ceiling(as.numeric(init_size_fraction) * 200)
init_design = if (init == "random") {
  generate_design_random(optim_instance$search_space, n = init_design_size)$data
} else if (init == "lhs") {
  generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
} else if (init == "sobol") {
  generate_design_sobol(optim_instance$search_space, n = init_design_size)$data
}

optim_instance$eval_batch(init_design)

learner = LearnerRegrRangerMbo$new()
learner$predict_type = "se"
learner$param_set$values$keep.inbag = TRUE

if (rf_type == "standard") {
  learner$param_set$values$se.method = "jack"
  learner$param_set$values$splitrule = "variance"
  learner$param_set$values$num.trees = 1000L
} else if (rf_type == "extratrees") {
  learner$param_set$values$se.method = "jack"
  learner$param_set$values$splitrule = "extratrees"
  learner$param_set$values$num.random.splits = 1L
  learner$param_set$values$num.trees = 1000L
} else if (rf_type == "smaclike_boot") {
  learner$param_set$values$se.method = "simple"
  learner$param_set$values$splitrule = "extratrees"
  learner$param_set$values$num.random.splits = 1L
  learner$param_set$values$num.trees = 10L
  learner$param_set$values$replace = TRUE
  learner$param_set$values$sample.fraction = 1
  learner$param_set$values$min.node.size = 1
  learner$param_set$values$mtry.ratio = 1
} else if (rf_type == "smaclike_no_boot") {
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
surrogate$param_set$values$catch_errors = TRUE

acq_optimizer = if (acqopt == "RS_1000") {
  AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
} else if (acqopt == "RS") {
  AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 20000L))
} else if (acqopt == "FS") {
  n_repeats = 2L
  maxit = 9L
  batch_size = ceiling((20000L / n_repeats) / (1 + maxit)) # 1000L
  AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 20000L))
} else if (acqopt == "LS") {
  optimizer = OptimizerChain$new(list(opt("local_search", n_points = 100L), opt("random_search", batch_size = 1000L)), terminators = list(trm("evals", n_evals = 10000L), trm("evals", n_evals = 10000L)))
  acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 20000L))
  acq_optimizer$param_set$values$warmstart = TRUE
  acq_optimizer$param_set$values$warmstart_size = "all"
  acq_optimizer
}
acq_optimizer$param_set$values$catch_errors = FALSE

acq_function = if (acqf == "EI" && log_scale) {
  AcqFunctionLogEI$new()
} else if (acqf == "EI" && !log_scale) {
  AcqFunctionEI$new()
} else if (acqf == "CB") {
  AcqFunctionCB$new(lambda = as.numeric(lambda))
} else if (acqf == "PI") {
  AcqFunctionPI$new()
} else if (acqf == "Mean") {
  AcqFunctionMean$new()
}

if (!log_scale) {
  bayesopt_ego(
    optim_instance,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    random_interleave_iter = random_interleave_iter,
    init_design_size = init_design_size)
} else {
  bayesopt_ego_log(
    optim_instance,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    random_interleave_iter = random_interleave_iter,
    init_design_size = init_design_size)
}

target = optim_instance$archive$cols_y
best = optim_instance$archive$best()[[target]]
data.table(best = best, target = target, problem = job$prob.name, id = id, repl = job$repl)


# issue 1
# Error in .subset2(x, i, exact = exact) :
#   attempt to select less than one element in get1index

# log_scale = TRUE
# init = "random"
# init_size_fraction = 0.25
# random_interleave_iter = 0
# rf_type = "standard"
# acqf = "EI"
# acqopt = "RS_1000"
# lambda = NA_real_
# id = 1L
# config_hash = "88169553-ad40-4225-98ef-0579f4207f43"

# xs = list(log_scale = TRUE, init = "random", init_size_fraction = 0.25,
#   random_interleave_iter = 0, rf_type = "standard", acqf = "EI",
#   acqopt = "RS_1000", lambda = NA_real_, id = 1L, config_hash = "88169553-ad40-4225-98ef-0579f4207f43")

# invoke(run, .args = xs)
