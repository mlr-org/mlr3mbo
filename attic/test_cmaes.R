library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3mbo)
library(mlr3pipelines)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)
library(reticulate)
library(mlr3learners)
library(yahpogym)

data.table::setDTthreads(1L)


use_condaenv("yahpo_gym", required = TRUE)
yahpo_gym = import("yahpo_gym")

setup = mlr3misc::rowwise_table(
     ~benchmark, ~scenario, ~instance, ~target_variable, ~direction,
     "pure_numeric", "lcbench", "167168", "val_accuracy", "maximize",
     "pure_numeric", "lcbench", "189873", "val_accuracy", "maximize",
     "pure_numeric", "lcbench", "189906", "val_accuracy", "maximize",
     "pure_numeric", "rbv2_rpart", "14", "acc", "maximize",
     "pure_numeric", "rbv2_rpart", "40499", "acc", "maximize",
     "pure_numeric", "rbv2_xgboost", "12", "acc", "maximize",
     "pure_numeric", "rbv2_xgboost", "1501", "acc", "maximize",
     "pure_numeric", "rbv2_xgboost", "40499", "acc", "maximize"
)


benchmark = BenchmarkSet$new("lcbench", instance = "167168")
benchmark$subset_codomain("val_accuracy")
objective = benchmark$get_objective("167168", multifidelity = FALSE)
search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)

optim_instance = oi(objective, search_space = search_space, terminator = trm("none"))

init = generate_design_lhs(search_space, n = 50)$data # 10% to full budget

optim_instance$eval_batch(init)

learner =  lrn("regr.ranger",
      num.trees = 100L,
      se.method = "simple",
      splitrule = "variance",
      predict_type = "se",
      keep.inbag = TRUE,
      sample.fraction = 1,
      min.node.size = 3,
      min.bucket = 3,
      mtry.ratio = 5/6
    )

surrogate = SurrogateLearner$new(learner)
acq_function = AcqFunctionEI$new()

surrogate$archive = optim_instance$archive
acq_function$surrogate = surrogate
acq_function$surrogate$update()
acq_function$update()


sigma = (objective$domain$upper - objective$domain$lower) / 3

# cmaes
acq_optimizer_cmaes = AcqOptimizerCmaes$new()
acq_optimizer_cmaes$param_set$set_values(
  maxeval = 1000L,
  restart_strategy = "none",
  n_restarts = 0L
)
acq_optimizer_cmaes$acq_function = acq_function
acq_optimizer_cmaes$optimize()





##########

library(Rlibcmaes)

cmaes = function(x0, optimFun, lower, upper, params=cmaEsParams(), cl=NULL) {
  stopifnot(all(upper > lower))

  # set default value for sigma
  if (is.null(params$sigma)) {
    params$sigma <- stats::median(upper-lower) / 4
  }

  optimFunBlock <- function(x) {
    browser()
    if (is.null(cl))
      return(apply(x,2,optimFun))
    else
      return(parallel::parApply(cl,x,2,optimFun))
  }
  Rlibcmaes::cmaesOptim(x0, params$sigma, optimFun, optimFunBlock,lower, upper, cmaAlgo = as.integer(params$cmaAlgorithm), lambda = ifelse(is.null(params$lambda),-1,params$lambda), maxEvals = params$maxEvals, xtol=params$xtol, ftol=params$ftol, traceFreq =params$trace, seed = params$seed, quietRun=params$quiet)
}


fun = get_private(acq_function)$.fun
constants = acq_function$constants$values
direction = acq_function$codomain$direction

wrapper = function(x) {
  xdt = set_names(as.data.table(t(xmat)), acq_function$domain$ids())
  res = mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
  res * acq_function$codomain$direction
}

x0 = set_names(as.numeric(generate_design_random(acq_function$domain, n = 1)$data), acq_function$domain$ids())


res = cmaes(x0 = x0, optimFun = wrapper, lower = acq_function$domain$lower, upper = acq_function$domain$upper, params = cmaEsParams(xtol = 1e-3,ftol = 1e-3))

res = cmaes(x0 = x0, optimFun = wrapper, lower = acq_function$domain$lower, upper = acq_function$domain$upper, params = cmaEsParams(cmaAlgorithm = cmaEsAlgo()$IPOP_CMAES,maxEvals=1e4))


wrapper = function(xmat) { #  fun, constants, direction
  xdt = set_names(as.data.table(t(xmat)), acq_function$domain$ids())
  res = mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
  res * direction
}

optimFunBlock = function(x) {
  wrapper(x)
}

lower = acq_function$domain$lower
upper = acq_function$domain$upper

res = Rlibcmaes::cmaesOptim(
  x0 = x0,
  sigma = median(upper - lower) / 4,
  optimFun = wrapper,
  optimFunBlock = optimFunBlock,
  lower = lower,
  upper = upper,
  cmaAlgo = as.integer(cmaEsAlgo()$IPOP_CMAES),
  lambda = -1,
  maxEvals = 1e4,
  xtol = 1e-3,
)

wrapper(res)


## random search
acq_optimizer_random_search = AcqOptimizer$new(opt("random_search", batch_size = 1000L), trm("evals", n_evals = 1000L))
acq_optimizer_random_search$acq_function = acq_function
res_random_search = acq_optimizer_random_search$optimize()

res_random_search



# cmaes
acq_optimizer_cmaes = AcqOptimizerCmaes$new()
acq_optimizer_cmaes$param_set$set_values(
  maxEvals = 30000L,
  xtol = 1e-4
)
acq_optimizer_cmaes$acq_function = acq_function
acq_optimizer_cmaes$optimize()
