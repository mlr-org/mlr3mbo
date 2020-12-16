#FIXME: should we pass the arvive here, too?

# most important controls:
# - the acqf, with its settings
# - the terminator, thats in objective
# - the design?
# - the optimizer?

# and why do we pass some tings in "control" and some separately?

#' @title Single Objective Bayesian Optimization
#'
#' @description
#' Function that executes a simple single-objective Bayesian optimization.
#' @template param_instance
#' @template param_acq_function
#' @template param_acq_optimizer
#' @param n_design (`int(1)`)\cr
#'   In case the `archive` inside the `instance` is empty, we generate a random initial design of `n_design` points.
#' @return [bbotk::Archive]
#'
#' @references
#' `r format_bib("jones_1998")`
#'
#' @export
bayesop_soo = function(instance, acq_function, acq_optimizer, n_design = 4 * instance$search_space$length) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstance")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  archive = instance$archive

  #FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = if(instance$search_space$has_deps) {
      generate_design_random(instance$search_space, n_design)$data
    } else {
      generate_design_lhs(instance$search_space, n_design)$data
    }
    instance$eval_batch(design)
  }

  acq_function$setup(archive) #setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    xydt = archive$data()
    # FIXME: maybe a tryCatch construction may be better here?
    # FIXME: catching errors during training the surrogate depends on whether we encapsulate
    acq_function$surrogate$update(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y)  # update surrogate model with new data
    sufficient_insample_performance = isTRUE(acq_function$surrogate$test_insample_performance)  # FIXME: do we also want to log if the performance is insufficient?
    # isTRUE currently due to NaN being possible as an insample performance if the update failed (we reset then)

    acq_function$update(archive)  # NOTE: necessary becaue we have to dertermine e.g. y_best for ei, there are possible other costy calculations that we just want to do once for each state. We might not want to do these calculation in acq_function$eval_dt() because this can get called several times during the optimization.
    # one more costy example would be AEI, where we ask the surrogate for the mean prediction of the points in the design
    # alternatively the update could be called by the AcqOptimizer (but he should not need to know about the archive, so then the archive also has to live in the AcqFun),

    xdt = if (sufficient_insample_performance) {
      xdt = acq_optimizer$optimize(acq_function)

      # FIXME: checking the (Gower) distance of the proposed point(s) to the previous point(s) is slightly more complicated; think about:
      # - how many points are proposed
      # - how many points were proposed previously
      # - what if we propose async
      acq_optimizer$xdt_fix_distance(xdt, previous_xdt = xydt[which(batch_nr == max(xydt$batch_nr)), archive$cols_x, with = FALSE], search_space = instance$search_space)
    } else {
      # lg$info("Proposing a randomly sampled point")  # FIXME: what about logging in general?
      SamplerUnif$new(instance$search_space)$sample(1L)$data  # NOTE: we always only propose a single random point in this case?
    }

    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(archive)) break
  }

  return(instance$archive)
}

if (FALSE) {
  set.seed(1)
  library(bbotk)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  obfun = ObjectiveRFun$new(
    fun = function(xs) sum(unlist(xs)^2),
    domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
    id = "test"
  )

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  surrogate$model$encapsulate = c(train = "evaluate", predict = "evaluate")
  acqfun = acq_function = AcqFunctionEI$new(surrogate = surrogate)
  acqopt = acq_optimizer = AcqOptimizerRandomSearch$new()

  bayesop_soo(instance, acqfun, acqopt)
}

if (FALSE) {
  set.seed(1)
  library(bbotk)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  obfun = ObjectiveRFun$new(
    fun = function(xs) {
      (xs$x1 - switch(xs$x2, "a" = 0, "b" = 1, "c" = 2)) %% xs$x3 + (if (xs$x4) xs$x1 else pi)
    },
    domain = ParamSet$new(list(
      ParamDbl$new("x1", -5, 5),
      ParamFct$new("x2", levels = c("a", "b", "c")),
      ParamInt$new("x3", 1, 2),
      ParamLgl$new("x4"))),
    id = "test"
  )

  terminator = trm("evals", n_evals = 100)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.ranger"))
  surrogate$model$param_set$values = list(se.method = "jack", keep.inbag = TRUE)
  surrogate$model$encapsulate = c(train = "evaluate", predict = "evaluate")
  acqfun = acq_function = AcqFunctionEI$new(surrogate = surrogate)
  acqopt = acq_optimizer = AcqOptimizerRandomSearch$new()

  bayesop_soo(instance, acqfun, acqopt)
}
