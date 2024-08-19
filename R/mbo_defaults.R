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
  if (inherits(instance, "OptimInstanceBatchSingleCrit")) {
    bayesopt_ego
  } else if (inherits(instance, "OptimInstanceBatchMultiCrit")) {
    bayesopt_smsego
  }
}

#' @title Default Gaussian Process
#'
#' @description
#' This is a helper function that constructs a default Gaussian Process [mlr3::LearnerRegr] which is for example used in
#' [default_surrogate].
#'
#' Constructs a Kriging learner \dQuote{"regr.km"} with kernel \dQuote{"matern5_2"}.
#' If \code{noisy = FALSE} (default) a small nugget effect is added \code{nugget.stability = 10^-8} to increase
#' numerical stability to hopefully prevent crashes of \CRANpkg{DiceKriging}.
#' If \code{noisy = TRUE} the nugget effect will be estimated with \code{nugget.estim = TRUE}.
#' If \code{noisy = TRUE} \code{jitter} is set to \code{TRUE} to circumvent a problem with \CRANpkg{DiceKriging} where
#' already trained input values produce the exact trained output.
#' In general, instead of the default \code{"BFGS"} optimization method we use rgenoud (\code{"gen"}), which is a hybrid
#' algorithm, to combine global search based on genetic algorithms and local search based on gradients.
#' This may improve the model fit and will less frequently produce a constant model prediction.
#'
#' @param noisy (logical(1))\cr
#'   Whether the learner will be used in a noisy objective function scenario.
#'   See above.
#' @return [mlr3::LearnerRegr]
#' @family mbo_defaults
#' @export
default_gp = function(noisy = FALSE) {
  assert_flag(noisy)
  require_namespaces("mlr3learners")
  require_namespaces("DiceKriging")
  require_namespaces("rgenoud")
  learner = mlr3learners::LearnerRegrKM$new()
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(covtype = "matern5_2", optim.method = "gen", control = list(trace = FALSE))
  )
  if (noisy) {
    learner$param_set$values = insert_named(learner$param_set$values, list(nugget.estim = TRUE, jitter = 1e-12))
  } else {
    learner$param_set$values = insert_named(learner$param_set$values, list(nugget.stability = 10^-8))
  }
  learner
}

#' @title Default Random Forest
#'
#' @description
#' This is a helper function that constructs a default random forest [mlr3::LearnerRegr] which is for example used in
#' [default_surrogate].
#'
#' Constructs a ranger learner \dQuote{"regr.ranger"} with \code{num.trees = 100}, \code{keep.inbag = TRUE} and
#' \code{se.method = "jack"}.
#'
#' @param noisy (logical(1))\cr
#'   Whether the learner will be used in a noisy objective function scenario.
#'   Currently has no effect.
#' @return [mlr3::LearnerRegr]
#' @family mbo_defaults
#' @export
default_rf = function(noisy = FALSE) {
  assert_flag(noisy)
  require_namespaces("mlr3learners")
  require_namespaces("ranger")
  learner = mlr3learners::LearnerRegrRanger$new()
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(num.trees = 100L, keep.inbag = TRUE, se.method = "jack")
  )
  learner
}

#' @title Default Surrogate
#'
#' @description
#' This is a helper function that constructs a default [Surrogate] based on properties of the
#' [bbotk::OptimInstance].
#'
#' For numeric-only (including integers) parameter spaces without any dependencies a Gaussian Process is constricted via
#' [default_gp()].
#' For mixed numeric-categorical parameter spaces, or spaces with conditional parameters a random forest is constructed via
#' [default_rf()].
#'
#' In any case, learners are encapsulated using \dQuote{"evaluate"}, and a fallback learner is set,
#' in cases where the surrogate learner errors.
#' Currently, the following learner is used as a fallback:
#' \code{lrn("regr.ranger", num.trees = 10L, keep.inbag = TRUE, se.method = "jack")}.
#'
#' If additionally dependencies are present in the parameter space, inactive conditional parameters
#' are represented by missing \code{NA} values in the training design data.
#' We simply handle those with an imputation method, added to the random forest, more
#' concretely we use \code{po("imputesample")} (for logicals) and \code{po("imputeoor")} (for anything else) from
#' package \CRANpkg{mlr3pipelines}.
#' Characters are always encoded as factors via \code{po("colapply")}.
#' Out of range imputation makes sense for tree-based methods and is usually hard to beat, see Ding et al. (2010).
#' In the case of dependencies, the following learner is used as a fallback:
#' \code{lrn("regr.featureless")}.
#'
#' If the instance is of class [bbotk::OptimInstanceBatchSingleCrit] the learner is wrapped as a
#' [SurrogateLearner].
#'
#' If the instance is of class [bbotk::OptimInstanceBatchMultiCrit] multiple deep clones of the learner are
#' wrapped as a [SurrogateLearnerCollection].
#'
#' @references
#' * `r format_bib("ding_2010")`
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @param learner (`NULL` | [mlr3::Learner]).
#'   If specified, this learner will be used instead of the defaults described above.
#' @param n_learner (`NULL` | `integer(1)`).
#'  Number of learners to be considered in the construction of the [SurrogateLearner] or [SurrogateLearnerCollection].
#'  If not specified will be based on the number of objectives as stated by the instance.
#' @return [Surrogate]
#' @family mbo_defaults
#' @export
default_surrogate = function(instance, learner = NULL, n_learner = NULL) {
  assert_multi_class(instance, c("OptimInstance", "OptimInstanceAsync"))
  assert_r6(learner, "Learner", null.ok = TRUE)
  assert_int(n_learner, lower = 1L, null.ok = TRUE)
  noisy = "noisy" %in% instance$objective$properties

  if (is.null(learner)) {
    is_mixed_space = !all(instance$search_space$class %in% c("ParamDbl", "ParamInt"))
    has_deps = nrow(instance$search_space$deps) > 0L
    learner = if (!is_mixed_space) {
      default_gp(noisy)
    } else {
      default_rf(noisy)
    }
    # stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    require_namespaces("ranger")
    fallback = mlr3learners::LearnerRegrRanger$new()
    fallback$param_set$values = insert_named(
      fallback$param_set$values,
      list(num.trees = 10L, keep.inbag = TRUE, se.method = "jack")
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
  if (inherits(instance, "OptimInstanceBatchSingleCrit")) {
    AcqFunctionEI$new()
  } else if (inherits(instance, "OptimInstanceBatchMultiCrit")) {
    AcqFunctionSmsEgo$new()
  }
}

#' @title Default Acquisition Function Optimizer
#'
#' @description
#' Chooses a default acquisition function optimizer.
#' Defaults to wrapping [bbotk::OptimizerBatchRandomSearch] allowing 10000 function evaluations (with a batch size of 1000) via a [bbotk::TerminatorEvals].
#'
#' @param acq_function ([AcqFunction]).
#' @return [AcqOptimizer]
#' @family mbo_defaults
#' @export
default_acqoptimizer = function(acq_function) {
  assert_r6(acq_function, classes = "AcqFunction")
  AcqOptimizer$new(optimizer = opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 10000L))  # FIXME: what do we use?
  # NOTE: adjust for single-objective vs. multi-objective acquisition function
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

