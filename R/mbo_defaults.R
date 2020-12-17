#' @title Defaults for OptimizerMbo
#'
#' @description
#' The following defaults are set for [`OptimizerMbo`] during opimization if the
#' respective fields are not set during initialization.
#'
#' * Optimization Loop: See [`default_loopfun`] \cr
#' * Acquisition Funciton: [`default_acqfun`] \cr
#'   Internally also sets the surrogate learner using [`default_surrogate`].
#' * Surrogate Learner: [`default_surrogate`] \cr
#' * Acqfun Optimizer: [`default_acq_optimizer`] \cr
#'
#' @name mbo_defaults
#' @family mbo_defaults
NULL

#' @title Default loopfun
#'
#' @description
#' Chooses a  default 'loopfun', i.e. the MBO flavour to be used for optimization.
#' For single-criteria optimization, defaults to [`bayesop_soo`].
#' For multi-criteria optimization, defaults to [`bayesopt_smsemoa`].
#'
#' @param instance [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @family mbo_defaults
#' @export
default_loopfun = function(instance) {
  if (inherits(instance, "OptimInstanceSingleCrit")) {
    bayesop_soo
  } else {
    bayesopt_smsemoa
  }
}

#' @title Generate default surrogate
#'
#' @description
#' This is a helper function that generates a default surrogate, based on properties of the objective
#' function and the selected infill criterion.
#'
#' For numeric-only (including integers) parameter spaces without any dependencies:
#' \itemize{
#' \item{A Kriging model \dQuote{regr.km} with kernel \dQuote{matern3_2} is created.}
#' \item{If the objective function is deterministic we add a small nugget effect (10^-8*Var(y),
#'   y is vector of observed outcomes in current design) to increase numerical stability to
#'   hopefully prevent crashes of DiceKriging.
#'   Whether the objective function is deterministic can be observed from the objective functions
#'   properties.}
#' \item{If the objective function is noisy the nugget effect will be estimated with
#'   \code{nugget.estim = TRUE}}
#'   Also \code{jitter} is set to \code{TRUE} to circumvent a problem with DiceKriging where already
#'   trained input values produce the exact trained output.
#' \item{Instead of the default \code{"BFGS"} optimization method we use rgenoud (\code{"gen"}),
#'   which is a hybrid algorithm, to combine global search based on genetic algorithms and local search
#'   based on gradients.
#'   This may improve the model fit and will less frequently produce a constant surrogate model.}
#' }
#'
#' For mixed numeric-categorical parameter spaces, or spaces with conditional parameters:
#' \itemize{
#' \item{A random regression forest \dQuote{regr.randomForest} with 500 trees is created.}
#' \item{The standard error of a prediction (if required by the infill criterion) is estimated
#'   by computing the jackknife-after-bootstrap.
#'   This is the \code{se.method = "jackknife"} option of the \dQuote{regr.randomForest} Learner.
#'   }
#' }
#' In any case, learners are encapsulated using "evaluate", and a fallback learner is set, in cases
#' where the surrogate learner errors. Currently, the following learner is used as a fallback:
#' ` GraphLearner$new(po("imputeoor") %>>% lrn("regr.ranger", num.trees = 20L, keep.inbag = TRUE))`.
#'
#' If additionally dependencies are present in the parameter space, inactive conditional parameters
#' are represented by missing \code{NA} values in the training design data.frame.
#' We simply handle those with an imputation method, added to the random forest, more concretely we
#' use `po("imputeoor")` from package \CRANpkg{mlr3pipelines}.
#' Both of these techniques make sense for tree-based methods and are usually hard to beat, see
#' Ding et.al. (2010).
#'
#' @references
#' `r format_bib("ding_2012")
#'
#' @param instance [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @param learner [mlr3::LearnerRegr] \cr
#'   The surrogate learner used in the acquisition function. Defaults to [`default_surrogate`].
#' @return [\code{Learner}]
#' @family mbo_defaults
#' @export
default_surrogate = function(instance, learner = NULL, n_objectives = NULL) {
  assert_r6(instance, "OptimInstance")
  assert_integer(n_objectives, null.ok=TRUE)

  if (is.null(learner)) {
    is_mixed_space = !all(instance$search_space$class %in% c("ParamDbl", "ParamInt"))
    has_deps = nrow(instance$search_space$deps) > 0L
    if (!is_mixed_space) {
      learner = lrn("regr.km", covtype = "matern3_2", optim.method = "gen")
      if ("deterministic" %in% instance$objective$properties) {
        insert_named(learner$param_set$values, list(nugget.stability = 10^-8))
      } else {
        insert_named(learner$param_set$values, list(nugget.estim = TRUE, jitter = TRUE))
      }
    } else {
      learner = lrn("regr.ranger", num.trees = 50L, keep.inbag = TRUE)
      # FIXME mrMBO: lrn("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE)
      # This currently does not work because mlr3's random forest does not have
      # se estimation.
    }
    # Stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    learner$fallback = lrn("regr.ranger", num.trees = 20L, keep.inbag = TRUE)

    if (has_deps) {
      require_namespaces("mlr3pipelines")
      learner = GraphLearner$new(po("imputeoor") %>>% learner)
      learner$fallback = lrn("regr.featureless")
    }
  }

  if (is.null(n_objectives)) {
    n_objectives = inst$objective$ydim
  }
  if (n_objectives == 1L) {
    surrogate = SurrogateSingleCritLearner$new(learner = learner)
  } else {
    if (!is.list(learner)) {
      learner = replicate(n_objectives, learner$clone())
    }
    assert_list(learner, len = n_objectives, types = "Learner")
    surrogate = SurrogateMultiCritLearners$new(learners = learner)
  }
  return(surrogate)
}

#' @title Default Acquisition Function
#'
#' @description
#' Chooses a  default 'acqfun', i.e. the criterion used to select future points.
#' If no learner is provided, internally calls [`default_surrogate`] to select an
#' appropriate surrogate learner.
#' @param instance [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @param surrogate [[mlr3mbo::SurrogateSingleCritLearner]|[mlr3mbo::SurrogateMultiCritLearners]] \cr
#'   The surrogate used in the acquisition function. Defaults to [`default_surrogate`].
#' @family mbo_defaults
#' @export
default_acqfun = function(instance, surrogate = NULL) {
  assert_r6(instance, "OptimInstance")
  if (is.null(surrogate)) {
    surrogate = default_surrogate(instance)
  }
  AcqFunctionEI$new(surrogate = surrogate)
}

#' @title Default Acquisition Function Optimizer
#'
#' @description
#' Chooses a  default 'acq_optimizer'
#' Defaults to [`AcqOptimizerRandomSearch`].
#'
#' @param instance [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @family mbo_defaults
#' @export
default_acq_optimizer = function(instance) {
  assert_r6(instance, "OptimInstance")
  AcqOptimizerRandomSearch$new()
}
