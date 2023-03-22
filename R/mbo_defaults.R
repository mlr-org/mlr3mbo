#' @title Defaults for OptimizerMbo
#' @name mbo_defaults
#'
#' @description
#' The following defaults are set for [OptimizerMbo] during optimization if the
#' respective fields are not set during initialization.
#'
#' * Optimization Loop: [default_loop_function]\cr
#' * Surrogate: [default_surrogate]\cr
#' * Acquisition Function: [default_acqfunction]\cr
#' * Acqfun Optimizer: [default_acqoptimizer]\cr
#' * Result Assigner: [default_result_assigner]\cr
#'
#' @family mbo_defaults
NULL

#' @title Default Loop Function
#'
#' @description
#' Chooses a default [loop_function], i.e. the Bayesian Optimization flavor to be used for optimization.
#' For single-objective optimization, defaults to [bayesopt_ego].
#' For multi-objective optimization, defaults to [bayesopt_smsego].
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @return [loop_function]
#' @family mbo_defaults
#' @export
default_loop_function = function(instance) {
  if (inherits(instance, "OptimInstanceSingleCrit")) {
    bayesopt_ego
  } else if (inherits(instance, "OptimInstanceMultiCrit")) {
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
#' \item{The standard error of a prediction (if required by the infill criterion) is estimated via jackknife.
#'   This is the \code{se.method = "jack"} option of the \dQuote{"regr.ranger"} learner (default).
#'   }
#' }
#' In any case, learners are encapsulated using \dQuote{"evaluate"}, and a fallback learner is set,
#' in cases where the surrogate learner errors. Currently, the following learner is used as a fallback:
#' \code{lrn("regr.ranger", num.trees = 20L, keep.inbag = TRUE, se.method = "jack")}.
#'
#' If additionally dependencies are present in the parameter space, inactive conditional parameters
#' are represented by missing \code{NA} values in the training design data.
#' We simply handle those with an imputation method, added to the ranger random forest, more
#' concretely we use \code{po("imputesample")} (for logicals) and \code{po("imputeoor")} (for anything else) from
#' package \CRANpkg{mlr3pipelines}.
#' Characters are always encoded as factors via \code{po("colapply")}.
#' Out of range imputation makes sense for tree-based methods and is usually hard to beat, see Ding et al. (2010).
#' In the case of dependencies, the following learner is used as a fallback:
#' \code{lrn("regr.featureless")}.
#'
#' If the instance is of class [bbotk::OptimInstanceSingleCrit] the learner is wrapped as a
#' [SurrogateLearner].
#'
#' If the instance is of class [bbotk::OptimInstanceMultiCrit] deep clones of the learner are
#' wrapped as a [SurrogateLearnerCollection].
#'
#' @references
#' * `r format_bib("ding_2010")`
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
        list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE))
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
        list(num.trees = 500L, keep.inbag = TRUE, se.method = "jack")
      )
    }
    # Stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    require_namespaces("ranger")
    fallback = mlr3learners::LearnerRegrRanger$new()
    fallback$param_set$values = insert_named(
      fallback$param_set$values,
      list(num.trees = 20L, keep.inbag = TRUE, se.method = "jack")
    )
    learner$fallback = fallback

    if (has_deps) {
      require_namespaces("mlr3pipelines")
      learner = mlr3pipelines::GraphLearner$new(
        mlr3pipelines::"%>>%"(
          mlr3pipelines::"%>>%"(
            mlr3pipelines::po("imputesample", affect_columns = mlr3pipelines::selector_type("logical")),
              mlr3pipelines::"%>>%"(
                mlr3pipelines::po("imputeoor", multiplier = 3, affect_columns = mlr3pipelines::selector_type(c("integer", "numeric", "character", "factor", "ordered"))),
                mlr3pipelines::po("colapply", applicator = as.factor, affect_columns = mlr3pipelines::selector_type("character"))
              )
          ),
          learner
        )
      )
      learner$encapsulate[c("train", "predict")] = "evaluate"
      learner$fallback = LearnerRegrFeatureless$new()
    }
  }

  if (is.null(n_learner)) n_learner = length(instance$archive$cols_y)
  if (n_learner == 1L) {
    SurrogateLearner$new(learner)
  } else  {
    learners = replicate(n_learner, learner$clone(deep = TRUE), simplify = FALSE)
    SurrogateLearnerCollection$new(learners)
  }
}

#' @title Default Acquisition Function
#'
#' @description
#' Chooses a default acquisition function, i.e. the criterion used to propose future points.
#' For single-objective optimization, defaults to [mlr_acqfunctions_ei].
#' For multi-objective optimization, defaults to [mlr_acqfunctions_smsego].
#'
#' @param instance ([bbotk::OptimInstance]).
#' @return [AcqFunction]
#' @family mbo_defaults
#' @export
default_acqfunction = function(instance) {
  assert_r6(instance, classes = "OptimInstance")
  if (inherits(instance, "OptimInstanceSingleCrit")) {
    AcqFunctionEI$new()
  } else if (inherits(instance, "OptimInstanceMultiCrit")) {
    AcqFunctionSmsEgo$new()
  }
}

#' @title Default Acquisition Function Optimizer
#'
#' @description
#' Chooses a default acquisition function optimizer.
#' Defaults to wrapping [bbotk::OptimizerRandomSearch] allowing 10000 function evaluations (with a batch size of 1000) via a [bbotk::TerminatorEvals].
#'
#' @param acq_function ([AcqFunction]).
#' @return [AcqOptimizer]
#' @family mbo_defaults
#' @export
default_acqoptimizer = function(acq_function) {
  assert_r6(acq_function, classes = "AcqFunction")
  AcqOptimizer$new(optimizer = opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 10000L))  # FIXME: what do we use
}

#' @title Default Result Assigner
#'
#' @description
#' Chooses a default result assigner.
#' Defaults to [ResultAssignerArchive].
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @return [ResultAssigner]
#' @family mbo_defaults
#' @export
default_result_assigner = function(instance) {
  assert_r6(instance, classes = "OptimInstance")
  ResultAssignerArchive$new()
}

