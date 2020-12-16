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
#' @param inst [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @family mbo_defaults
#' @export
default_loopfun = function(inst) {
  if (inherits(inst, "OptimInstanceSingleCrit")) {
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
#'
#' If additionally dependencies are present in the parameter space, inactive conditional parameters
#' are represented by missing \code{NA} values in the training design data.frame.
#' We simply handle those with an imputation method, added to the random forest, more concretely we
#' use `po("imputeoor")` from package \CRANpkg{mlr3pipelines}.
#' Both of these techniques make sense for tree-based methods and are usually hard to beat, see
#' Ding et.al. (2010).
#'
#' @references
#' Ding, Yufeng, and Jeffrey S. Simonoff. An investigation of missing data methods for
#' classification trees applied to binary response data.
#' Journal of Machine Learning Research 11.Jan (2010): 131-170.
#'
#'
#' @param inst [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @return [\code{Learner}]
#' @family mbo_defaults
#' @export
default_surrogate = function(inst) {

  is_mixed_space = !all(inst$search_space$class %in% c("ParamDbl", "ParamInt"))
  has_deps = nrow(inst$search_space$deps) > 0L

  if (is_mixed_space) {
    l = lrn("regr.km", covtype = "matern3_2", optim.method = "gen")
    if ("deterministic" %in% inst$objective$properties) {
      l = insert_named(l$param_set$values, list(nugget.stability = 10^-8))
    } else {
      l = insert_named(l$param_set$values, list(nugget.estim = TRUE, jitter = TRUE))
    }
  } else {
    l = lrn("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE)
  }

  if (has_deps) {
    l = GraphLearner$new(po("imputeoor") %>>% l)
  }

  return(l)
}

#' @title Default Acquisition Function
#'
#' @description
#' Chooses a  default 'acqfun', i.e. the criterion used to select future points.
#' If no learner is provided, internally calls [`default_surrogate`] to select an
#' appropriate surrogate learner.
#' @param inst [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @param learner [mlr3::LearnerRegr] \cr
#'   The surrogate learner used in the acquisition function. Defaults to [`default_surrogate`].
#' @family mbo_defaults
#' @export
default_acqfun = function(inst, learner = NULL) {
  if (is.null(learner)) {
    learner = default_surrogate(inst)
  }

  # FIXME Depending on ydim will not work for parEGO
  if (inst$objective$ydim == 1L) {
    surrogate = SurrogateSingleCritLearner$new(learner = learner)
    AcqFunctionEI$new(surrogate = surrogate)
  } else {
    if (!is.list(learner)) {
      learner = map(seq_len(inst$objective$ydim), learner$clone())
    }
    surrogate = SurrogateMultiCritLearner$new(learners = learner)
    # FIXME: This is a little odd, since loopfun is only a function.
    AcqFunctionSmsEmoa$new(surrogate = surrogate)
  }
}

#' @title Default Acquisition Function Optimizer
#'
#' @description
#' Chooses a  default 'acq_optimizer'
#' Defaults to [`AcqOptimizerRandomSearch`].
#'
#' @param inst [bbotk::OptimInstance] \cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @family mbo_defaults
#' @export
default_acq_optimizer = function(inst) {
  AcqOptimizerRandomSearch$new()
}
