#' @title Defaults for OptimizerMbo
#' @name mbo_defaults
#'
#' @description
#' The following defaults are set for [OptimizerMbo] during optimization if the respective fields are not set during initialization.
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
  } else {
    stopf("There are no loop functions for %s.", class(instance)[1L])
  }
}

#' @title Default Gaussian Process
#'
#' @description
#' This is a helper function that constructs a default Gaussian Process [mlr3learners::LearnerRegrKM] which is for example used in [default_surrogate()].
#'
#' Constructs a Kriging learner `"regr.km"` with kernel `"matern5_2"`.
#' If `noisy = FALSE` (default) a small nugget effect is added `nugget.stability = 10^-8` to increase numerical stability to hopefully prevent crashes of `DiceKriging`.
#' If `noisy = TRUE` the nugget effect will be estimated with `nugget.estim = TRUE`.
#' If `noisy = TRUE` `jitter` is set to `TRUE` to circumvent a problem with `DiceKriging` where already trained input values produce the exact trained output.
#' In general, instead of the default `"BFGS"` optimization method we use rgenoud (`"gen"`), which is a hybrid algorithm, to combine global search based on genetic algorithms and local search based on gradients.
#' This may improve the model fit and will less frequently produce a constant model prediction.
#'
#' @param noisy (`logical(1)`)\cr
#'   Whether the learner will be used in a noisy objective function scenario.
#'   See above.
#'
#' @return [mlr3learners::LearnerRegrKM]
#' @family mbo_defaults
#' @export
default_gp = function(noisy = FALSE) {
  assert_flag(noisy)
  require_namespaces("mlr3learners")
  require_namespaces("DiceKriging")
  require_namespaces("rgenoud")

  learner = lrn("regr.km",
    predict_type = "se",
    control = list(trace = FALSE),
    optim.method = "gen",
    covtype = "matern5_2",
    scaling = FALSE
  )

  if (noisy) {
    learner$param_set$set_values(nugget.estim = TRUE, jitter = 1e-12)
  } else {
    learner$param_set$set_values(nugget.stability = 10^-8)
  }
  learner
}

#' @title Default Random Forest
#'
#' @description
#' This is a helper function that constructs a default random forest [mlr3learners::LearnerRegrRanger] which is for example used in [default_surrogate()].
#'
#' @return [mlr3learners::LearnerRegrRanger]
#' @family mbo_defaults
#' @export
default_rf = function(noisy = FALSE) {
  assert_flag(noisy)
  require_namespaces("mlr3learners")
  require_namespaces("ranger")
  lrn("regr.ranger",
    num.trees = 500L,
    se.method = "jack",
    splitrule = "variance",
    predict_type = "se",
    keep.inbag = TRUE,
    sample.fraction = 1,
    min.node.size = 3,
    min.bucket = 3,
    mtry.ratio = 5 / 6
  )
}

#' @title Default Surrogate
#'
#' @description
#' This is a helper function that constructs a default [Surrogate] based on properties of the [bbotk::OptimInstance].
#'
#' For purely numeric (including integers) parameter spaces without any dependencies a Gaussian Process is constricted via [default_gp()].
#' For mixed numeric-categorical parameter spaces, or spaces with conditional parameters a random forest is constructed via [default_rf()].
#'
#' In any case, learners are encapsulated using `"evaluate"`, and a fallback learner is set, in cases where the surrogate learner errors.
#' Currently, the following learner is used as a fallback: `lrn("regr.ranger", num.trees = 10L, keep.inbag = TRUE, se.method = "jack")`.
#'
#' If additionally dependencies are present in the parameter space, inactive conditional parameters are represented by missing `NA` values in the training design data.
#' We simply handle those with the internal `NA` handling method `na.action = "na_learn` of \CRANpkg{ranger}.
#'
#' If `n_learner` is `1`, the learner is wrapped as a [SurrogateLearner].
#' Otherwise, if `n_learner` is larger than `1`, multiple deep clones of the learner are wrapped as a [SurrogateLearnerCollection].
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   An object that inherits from [bbotk::OptimInstance].
#' @param learner (`NULL` | [mlr3::Learner]).
#'   If specified, this learner will be used instead of the defaults described above.
#' @param n_learner (`NULL` | `integer(1)`).
#'  Number of learners to be considered in the construction of the [Surrogate].
#'  If not specified will be based on the number of objectives as stated by the instance.
#' @param force_random_forest (`logical(1)`).
#'  If `TRUE`, a random forest is constructed even if the parameter space is purely numeric.
#' @return [Surrogate]
#' @family mbo_defaults
#' @export
default_surrogate = function(instance, learner = NULL, n_learner = NULL, force_random_forest = FALSE) {
  assert_multi_class(instance, c("OptimInstance", "OptimInstanceBatch", "OptimInstanceAsync"))
  assert_r6(learner, "Learner", null.ok = TRUE)
  assert_int(n_learner, lower = 1L, null.ok = TRUE)
  noisy = "noisy" %in% instance$objective$properties
  output_trafo = NULL

  if (is.null(learner)) {
    require_namespaces(c("ranger", "mlr3learners"))

    learner = if (instance$search_space$all_numeric && !instance$search_space$has_deps && !force_random_forest) {
      output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
      default_gp(noisy)
    } else {
      default_rf(noisy)
    }

    fallback = lrn("regr.ranger", num.trees = 10L, keep.inbag = TRUE, se.method = "jack", predict_type = "se")
    learner$encapsulate("evaluate", fallback)
  }

  if (is.null(n_learner)) n_learner = length(instance$archive$cols_y)

  if (n_learner == 1L) {
    SurrogateLearner$new(learner, output_trafo = output_trafo)
  } else {
    learners = replicate(n_learner, learner$clone(deep = TRUE), simplify = FALSE)
    SurrogateLearnerCollection$new(learners, output_trafo = output_trafo)
  }
}

#' @title Default Acquisition Function
#'
#' @description
#' Chooses a default acquisition function, i.e. the criterion used to propose future points.
#' For synchronous single-objective optimization and a purely numeric parameter space, defaults to [mlr_acqfunctions_ei].
#' For synchronous single-objective optimization and a mixed numeric-categorical parameter space, defaults to [mlr_acqfunctions_cb] with `lambda = 1`.
#' For synchronous multi-objective optimization, defaults to [mlr_acqfunctions_smsego].
#' For asynchronous single-objective optimization, defaults to [mlr_acqfunctions_stochastic_cb].
#'
#' @param instance ([bbotk::OptimInstance]).
#'   An object that inherits from [bbotk::OptimInstance].
#' @return [AcqFunction]
#' @family mbo_defaults
#' @export
default_acqfunction = function(instance) {
  assert_r6(instance, classes = "OptimInstance")
  if (inherits(instance, "OptimInstanceBatchSingleCrit") && instance$search_space$all_numeric) {
    AcqFunctionEILog$new()
  } else if (inherits(instance, "OptimInstanceBatchSingleCrit") && !instance$search_space$all_numeric) {
    AcqFunctionCB$new(lambda = 1)
  } else if (inherits(instance, "OptimInstanceAsyncSingleCrit")) {
    AcqFunctionStochasticCB$new()
  } else if (inherits(instance, "OptimInstanceBatchMultiCrit")) {
    AcqFunctionSmsEgo$new()
  } else if (inherits(instance, "OptimInstanceAsyncMultiCrit")) {
    stopf("Currently, there is no default acquisition function for %s.", class(instance)[1L])
  }
}

#' @title Default Acquisition Function Optimizer
#'
#' @description
#' Chooses a default acquisition function optimizer.
#' Defaults to wrapping [AcqOptimizerLocalSearch] with `n_searches = 10`, `n_steps = ceiling(100 * D^2 / 300)`, and `n_neighs = 30`, where `D` is the dimension of the search space.
#'
#' @param acq_function ([AcqFunction]).
#' @return [AcqOptimizer]
#' @family mbo_defaults
#' @export
default_acqoptimizer = function(acq_function, instance) {
  assert_r6(acq_function, classes = "AcqFunction")
  assert_instance(instance)

  dim = instance$search_space$length
  budget = 100L * dim^2
  acqo("local_search", acq_function = acq_function, n_searches = 10L, n_steps = ceiling(budget / 300L), n_neighs = 30L)
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

