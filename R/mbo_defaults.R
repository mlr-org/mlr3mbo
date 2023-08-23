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

#' @title Default Gaussian Process
#'
#' @description
#' This is a helper function that generates a default Gaussian Process [mlr3::LearnerRegr] which is for example used in
#' [default_surrogate].
#'
#' Constructs a Gaussian Process (Kriging) learner ([mlr3learners::LearnerRegrKM]) with kernel `"matern5_2"`.
#' If `noisy = FALSE` (default) a small nugget effect is added `nugget.stability = 10^-8` to increase
#' numerical stability to hopefully prevent crashes of [DiceKriging::km].
#' If `noisy = TRUE` the nugget effect will be estimated with `nugget.estim = TRUE`.
#' If `noisy = TRUE` `jitter` is set to `TRUE` to circumvent a problem with [DiceKriging::km] where
#' already trained input values produce the exact trained output.
#' In general, instead of the default `"BFGS"` optimization method we use rgenoud (`optim.method = "gen"`), which is a hybrid
#' algorithm, to combine global search based on genetic algorithms and local search based on gradients.
#' This may improve the model fit and will less frequently produce a constant model prediction.
#'
#' If `input_scaling = TRUE` (default is `FALSE`) the input features are preprocessed and transformed to the unit cube via
#' min-max scaling via [mlr3pipelines::PipeOpColApply] using the lower and upper bounds provided in the `search_space`.
#'
#' @param noisy (`logical(1)`)\cr
#'   Whether the learner will be used in a noisy objective function scenario.
#' @param input_scaling (`logical(1)`)\cr
#'   Whether the input features should be scaled to the unit cube.
#' @param search_space ([paradox::ParamSet])\cr
#'   Search space containing the lower and upper bounds of the input features, i.e., usually the search space of the
#'   [bbotk::OptimInstance] that is to be optimized.
#' @return [mlr3::LearnerRegr]
#' @family mbo_defaults
#' @export
default_gp = function(noisy = FALSE, input_scaling = FALSE, search_space = NULL) {
  assert_flag(noisy)
  assert_flag(input_scaling)
  assert_r6(search_space, classes = "ParamSet", null.ok = TRUE)
  if (input_scaling) {
    if (is.null(search_space)) {
      stop("`search_space` must be specified if `input_scaling = TRUE`.")
    }
  }
  require_namespaces("mlr3learners")
  require_namespaces("mlr3pipelines")
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

  if (input_scaling) {
    ids = search_space$ids()
    preprocess = mlr3pipelines::po("colapply",
      id = paste0("scale_", ids[1L]),
      affect_columns = mlr3pipelines::selector_name(ids[1L]),
      applicator = function(x) {
        (x - search_space$params[[ids[1L]]]$lower) / (search_space$params[[ids[1L]]]$upper - search_space$params[[ids[1L]]]$lower)
      }
    )
    if (length(ids) > 1L) {
      for (id in ids[-1L]) {
        preprocess = mlr3pipelines::"%>>%"(
          preprocess,
          mlr3pipelines::po("colapply",
            id = paste0("scale_", id),
            affect_columns = mlr3pipelines::selector_name(id),
            applicator = function(x) {
              (x - search_space$params[[id]]$lower) / (search_space$params[[id]]$upper - search_space$params[[id]]$lower)
            }
          )
        )
      }
    }
    learner = mlr3pipelines::GraphLearner$new(
      mlr3pipelines::"%>>%"(
        preprocess,
        learner
      )
    )
  }

  learner
}

#' @title Default Random Forest
#'
#' @description
#' This is a helper function that constructs a default random forest [mlr3::LearnerRegr] which is for example used in
#' [default_surrogate].
#'
#' Constructs a ranger random forest learner ([mlr3learners::LearnerRegrRanger]) with `num.trees = 100`, `keep.inbag = TRUE` and
#' `se.method = "jack"`.
#'
#' @param noisy (`logical(1)`)\cr
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
#' For numeric-only (including integers) search spaces without any dependencies a Gaussian Process is constructed via
#' [default_gp()].
#' For mixed numeric-categorical search spaces, or spaces with conditional parameters a random forest is constructed via
#' [default_rf()].
#'
#' In any case, learners are encapsulated using `"evaluate"`, and a fallback learner is set,
#' in cases where the surrogate learner errors.
#' Currently, the following learner is used as a fallback:
#' `lrn("regr.ranger", num.trees = 10L, keep.inbag = TRUE, se.method = "jack")`.
#'
#' If additionally dependencies are present in the search space, inactive conditional parameters
#' are represented by missing `NA` values in the training design data.
#' We simply handle those with an imputation method, added to the random forest, more
#' concretely we use [mlr3pipelines::PipeOpImputeSample] (for logicals) and [mlr3pipelines::PipeOpImputeOOR] (for anything else).
#' Characters are always encoded as factors via [mlr3pipelines::PipeOpColApply].
#' Out of range imputation makes sense for tree-based methods and is usually hard to beat, see Ding et al. (2010).
#' In the case of dependencies a `lrn("regr.featureless")` is used as a fallback.
#'
#' If the instance is of class [bbotk::OptimInstanceSingleCrit] the learner is wrapped as a
#' [SurrogateLearner].
#'
#' If the instance is of class [bbotk::OptimInstanceMultiCrit] multiple deep clones of the learner are
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
#'  If not specified this will be determined automatically based on the number of objectives as stated by the instance.
#' @return [Surrogate]
#' @family mbo_defaults
#' @export
default_surrogate = function(instance, learner = NULL, n_learner = NULL) {
  assert_r6(instance, "OptimInstance")
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

