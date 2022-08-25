#' @title Defaults for OptimizerMbo
#' @name mbo_defaults
#'
#' @description
#' The following defaults are set for [OptimizerMbo] during optimization if the
#' respective fields are not set during initialization.
#'
#' * Optimization Loop: [default_loopfun]\cr
#' * Surrogate: [default_surrogate]\cr
#' * Acquisition Function: [default_acqfun]\cr
#' * Acqfun Optimizer: [default_acqopt]\cr
#'
#' @family mbo_defaults
NULL

#' @title Default Loop Function
#'
#' @description
#' Chooses a default "loopfun", i.e. the MBO flavor to be used for optimization.
#' For single-criteria optimization, defaults to [bayesopt_ego].
#' For multi-criteria optimization, defaults to [bayesopt_smsego].
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @return loop `function`
#' @family mbo_defaults
#' @export
default_loopfun = function(instance) {
  if (test_r6(instance, classes = "OptimInstanceSingleCrit")) {
    bayesopt_ego
  } else if (test_r6(instance, classes = "OptimInstanceMultiCrit")) {
    bayesopt_smsego
  }
}

#' @title Default Surrogate
#'
#' @description
#' This is a helper function that generates a default [Surrogate] based on properties of the
#' [bbotk::OptimInstance].
#'
#' For numeric-only (including integers) parameter spaces without any dependencies:
#' \itemize{
#' \item{A Kriging model \dQuote{"regr.km"} with kernel \dQuote{"matern3_2"} is created.}
#' \item{If the objective function is deterministic we add a small nugget effect (10^-8*Var(y),
#'   y is vector of observed outcomes in current design) to increase numerical stability to
#'   hopefully prevent crashes of \CRANpkg{DiceKriging}.
#'   Whether the objective function is deterministic can be observed from the objective function's
#'   properties.}
#' \item{If the objective function is noisy the nugget effect will be estimated with
#'   \code{nugget.estim = TRUE}.}
#' \item{Also \code{jitter} is set to \code{TRUE} to circumvent a problem with \CRANpkg{DiceKriging}
#'   where already trained input values produce the exact trained output.
#'   Whether the objective function is noisy can be observed from the objective functions
#'   properties.}
#' \item{Instead of the default \code{"BFGS"} optimization method we use rgenoud (\code{"gen"}),
#'   which is a hybrid algorithm, to combine global search based on genetic algorithms and local search
#'   based on gradients.
#'   This may improve the model fit and will less frequently produce a constant surrogate model.}
#' }
#'
#' For mixed numeric-categorical parameter spaces, or spaces with conditional parameters:
#' \itemize{
#' \item{A ranger regression forest \dQuote{"regr.ranger"} with 500 trees is created.}
#' \item{The standard error of a prediction (if required by the infill criterion) is estimated
#'   by computing the infinitesimal jackknife.
#'   This is the \code{se.method = "infjack"} option of the \dQuote{"regr.ranger"} learner (default).
#'   }
#' }
#' In any case, learners are encapsulated using \dQuote{"evaluate"}, and a fallback learner is set,
#' in cases where the surrogate learner errors. Currently, the following learner is used as a fallback:
#' \code{lrn("regr.ranger", num.trees = 20L, keep.inbag = TRUE)}.
#'
#' If additionally dependencies are present in the parameter space, inactive conditional parameters
#' are represented by missing \code{NA} values in the training design data.
#' We simply handle those with an imputation method, added to the ranger random forest, more
#' concretely we use \code{po("imputesample")} and \code{po("imputeoor")} from package \CRANpkg{mlr3pipelines}.
#' Both of these techniques make sense for tree-based methods and are usually hard to beat, see Ding et al. (2010).
#' In the case of dependencies, the following learner is used as a fallback:
#' \code{lrn("regr.featureless")}.
#'
#' If the instance is of class [bbotk::OptimInstanceSingleCrit] the learner is wrapped as a
#' [SurrogateLearner].
#'
#' If the instance is of class [bbotk::OptimInstanceMultiCrit] deep clones of the learner are
#' wrapped as a [SurrogateLearners].
#'
#' @references
#' `r format_bib("ding_2010")`
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @param learner (`NULL` | [mlr3::Learner]).
#' @param n_learner (`NULL` | `integer(1)`).
#' @return [Surrogate]
#' @family mbo_defaults
#' @export
default_surrogate = function(instance, learner = NULL, n_learner = NULL) {
  assert_r6(instance, "OptimInstance")
  assert_r6(learner, "Learner", null.ok = TRUE)
  assert_int(n_learner, lower = 1L, null.ok = TRUE)

  if (is.null(learner)) {
    is_mixed_space = !all(instance$search_space$class %in% c("ParamDbl", "ParamInt"))
    has_deps = nrow(instance$search_space$deps) > 0L
    require_namespaces("mlr3learners")
    if (!is_mixed_space) {
      require_namespaces("DiceKriging")
      learner = mlr3learners::LearnerRegrKM$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(covtype = "matern3_2", optim.method = "gen")
      )
      if ("noisy" %in% instance$objective$properties) {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.estim = TRUE, jitter = 1e-12))
      } else {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.stability = 10^-8))
      }
    } else {
      require_namespaces("ranger")
      learner = mlr3learners::LearnerRegrRanger$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(num.trees = 500L, keep.inbag = TRUE)
      )
    }
    # Stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    require_namespaces("ranger")
    fallback = mlr3learners::LearnerRegrRanger$new()
    fallback$param_set$values = insert_named(
      learner$param_set$values,
      list(num.trees = 500L, keep.inbag = TRUE)
    )
    learner$fallback = fallback

    if (has_deps) {
      require_namespaces("mlr3pipelines")
      learner = mlr3pipelines::GraphLearner$new(mlr3pipelines::'%>>%'(mlr3pipelines::'%>>%'(mlr3pipelines::po("imputesample", affect_columns = mlr3pipelines::selector_type("logical")), mlr3pipelines::po("imputeoor")), learner))
      learner$encapsulate[c("train", "predict")] = "evaluate"
      learner$fallback = lrn("regr.featureless")
    }
  }

  if (is.null(n_learner)) n_learner = length(instance$archive$cols_y)
  if (n_learner == 1L) {
    SurrogateLearner$new(learner)
  } else  {
    learners = replicate(n_learner, learner$clone(deep = TRUE), simplify = FALSE)
    SurrogateLearners$new(learners)
  }
}

#' @title Default Acquisition Function
#'
#' @description
#' Chooses a default acquisition function, i.e. the criterion used to propose future points.
#' @param instance ([bbotk::OptimInstance]).
#' @return [AcqFunction]
#' @family mbo_defaults
#' @export
default_acqfun = function(instance) {
  assert_r6(instance, classes = "OptimInstance")
  if (testR6(instance, classes = "OptimInstanceSingleCrit")) {
    AcqFunctionEI$new()
  } else if (testR6(instance, classes = "OptimInstanceMultiCrit")) {
    AcqFunctionSmsEgo$new()
  }
}

#' @title Default Acquisition Function Optimizer
#'
#' @description
#' Chooses a default acquisition function optimizer.
#' Defaults to wrapping [bbotk::OptimizerRandomSearch] allowing 1000 function evaludations.
#'
#' @param acq_function ([AcqFunction]).
#' @return [AcqOptimizer]
#' @family mbo_defaults
#' @export
default_acqopt = function(acq_function) {
  assert_r6(acq_function, classes = "AcqFunction")
  AcqOptimizer$new(optimizer = opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))  # FIXME: what do we use
}

