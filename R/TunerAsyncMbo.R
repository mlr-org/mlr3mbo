#' @title TunerAsync using Asynchronous Model Based Optimization
#'
#' @include OptimizerAsyncMbo.R
#' @name mlr_tuners_async_mbo
#'
#' @description
#' `TunerAsyncMbo` class that implements Asynchronous Model Based Optimization (AMBO).
#' This is a minimal interface internally passing on to [OptimizerAsyncMbo].
#' For additional information and documentation see [OptimizerAsyncMbo].
#'
#' Currently, only single-objective optimization is supported and `TunerAsyncMbo` is considered an experimental feature and API might be subject to changes.
#'
#' @section Parameters:
#' \describe{
#' \item{`initial_design`}{`data.table::data.table()`\cr
#'   Initial design of the optimization.
#'   If `NULL`, a design of size `design_size` is generated with the specified `design_function`.
#'   Default is `NULL`.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the initial design if it is to be generated.
#'   Default is `100`.}
#' \item{`design_function`}{`character(1)`\cr
#'   Sampling function to generate the initial design.
#'   Can be `random` [paradox::generate_design_random], `lhs` [paradox::generate_design_lhs], or `sobol` [paradox::generate_design_sobol].
#'   Default is `sobol`.}
#' \item{`n_workers`}{`integer(1)`\cr
#'   Number of parallel workers.
#'   If `NULL`, all rush workers specified via [rush::rush_plan()] are used.
#'   Default is `NULL`.}
#' }
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rush") &
#'     requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'
#'   if (redis_available()) {
#'
#'     library(mlr3)
#'     library(mlr3tuning)
#'
#'     # single-objective
#'     task = tsk("wine")
#'     learner = lrn("classif.rpart", cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE))
#'     resampling = rsmp("cv", folds = 3)
#'     measure = msr("classif.acc")
#'
#'     instance = TuningInstanceAsyncSingleCrit$new(
#'       task = task,
#'       learner = learner,
#'       resampling = resampling,
#'       measure = measure,
#'       terminator = trm("evals", n_evals = 10))
#'
#'     mirai::daemons(2)
#'     rush::rush_plan(n_workers=2, worker_type = "remote")
#'
#'     tnr("async_mbo", design_size = 4, n_workers = 2)$optimize(instance)
#'     mirai::daemons(0)
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
TunerAsyncMbo = R6Class("TunerAsyncMbo",
  inherit = mlr3tuning::TunerAsyncFromOptimizerAsync,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `surrogate`, `acq_function`, `acq_optimizer`, and `result_assigner`, see `?mbo_defaults`.
    #'
    #' Note that all the parameters below are simply passed to the [OptimizerAsyncMbo] and
    #' the respective fields are simply (settable) active bindings to the fields of the [OptimizerAsyncMbo].
    #'
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @template param_result_assigner
    #' @param param_set ([paradox::ParamSet])\cr
    #'  Set of control parameters.
    initialize = function(surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, param_set = NULL) {
      optimizer = OptimizerAsyncMbo$new(surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, param_set = param_set)

      super$initialize(optimizer = optimizer, man = "mlr3mbo::TunerAsyncMbo")
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), if (is.na(self$label)) "" else paste0(": ", self$label))
      #catn(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catn(str_indent("* Parameter classes:", self$param_classes))
      catn(str_indent("* Properties:", self$properties))
      catn(str_indent("* Packages:", self$packages))
      catn(str_indent("* Surrogate:", if (is.null(self$surrogate)) "-" else self$surrogate$print_id))
      catn(str_indent("* Acquisition Function:", if (is.null(self$acq_function)) "-" else class(self$acq_function)[1L]))
      catn(str_indent("* Acquisition Function Optimizer:", if (is.null(self$acq_optimizer)) "-" else self$acq_optimizer$print_id))
      catn(str_indent("* Result Assigner:", if (is.null(self$result_assigner)) "-" else class(self$result_assigner)[1L]))
    },

    #' @description
    #' Reset the tuner.
    #' Sets the following fields to `NULL`:
    #' `surrogate`, `acq_function`, `acq_optimizer`, `result_assigner`
    #' Resets parameter values `design_size` and `design_function` to their defaults.
    reset = function() {
      private$.optimizer$reset()
    }
  ),

  active = list(
    #' @template field_surrogate
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$surrogate
      } else {
        private$.optimizer$surrogate = assert_r6(rhs, classes = "Surrogate", null.ok = TRUE)
      }
    },

    #' @template field_acq_function
    acq_function = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$acq_function
      } else {
        private$.optimizer$acq_function = assert_r6(rhs, classes = "AcqFunction", null.ok = TRUE)
      }
    },

    #' @template field_acq_optimizer
    acq_optimizer = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$acq_optimizer
      } else {
        private$.optimizer$acq_optimizer = assert_r6(rhs, classes = "AcqOptimizer", null.ok = TRUE)
      }
    },

    #' @template field_result_assigner
    result_assigner = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$result_assigner
      } else {
        private$.optimizer$result_assigner = assert_r6(rhs, classes = "ResultAssigner", null.ok = TRUE)
      }
    },

    #' @template field_param_classes
    param_classes = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$param_classes
      } else {
        stop("$param_classes is read-only.")
      }
    },

    #' @template field_properties
    properties = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$properties
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_packages
    packages = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$packages
      } else {
        stop("$packages is read-only.")
      }
    }
  )
)

#' @include aaa.R
tuners[["async_mbo"]] = TunerAsyncMbo
