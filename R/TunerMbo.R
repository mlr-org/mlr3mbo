#' @title Tuner using Bayesian Optimization
#'
#' @name mlr_tuners_mbo
#'
#' @description
#' Tune hyperparameters using Bayesian Optimization.
#' This is a minimal interface internally passing on to [OptimizerMbo].
#' For additional information and documentation see [OptimizerMbo].
#'
#' @export
TunerMbo = R6Class("TunerMbo",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `loop_function`, `surrogate`, `acq_function` and `acq_optimizer`, see `mbo_defaults`.
    #'
    #' Note that all the parameters below are simply passed to the [OptimizerMbo] and
    #' the respective fields are simply (settable) active bindings to the fields of the [OptimizerMbo].
    #'
    #' @template param_loop_function
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @template param_args
    #' @template param_result_function
    initialize = function(loop_function = NULL, surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, args = NULL, result_function = NULL) {
      super$initialize(optimizer = OptimizerMbo$new(loop_function = loop_function, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, args = args, result_function = result_function), man = "mlr3mbo::TunerMbo")
    }
  ),

  active = list(

    #' @template field_loop_function
    loop_function = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$loop_function
      } else {
        private$.optimizer$loop_function = assert_function(rhs, null.ok = TRUE)
      }
    },

    #' @template field_surrogate
    surrogate = function(rhs) {
     if (missing(rhs)) {
        private$.optimizer$surrogate
      } else {
        private$.optimizer$surrogate = assert_r6(rhs, "Surrogate", null.ok = TRUE)
      }
    },

    #' @template field_acq_function
    acq_function = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$acq_function
      } else {
        private$.optimizer$acq_function = assert_r6(rhs, "AcqFunction", null.ok = TRUE)
      }
    },

    #' @template field_acq_optimizer
    acq_optimizer = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$acq_optimizer
      } else {
        private$.optimizer$acq_optimizer = assert_r6(rhs, "AcqOptimizer", null.ok = TRUE)
      }
    },

    #' @template field_args
    args = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$args
      } else {
        private$.optimizer$args = assert_list(rhs, names = "named", null.ok = TRUE)
      }
    },

    #' @template field_result_function
    result_function = function(rhs) {
     if (missing(rhs)) {
        private$.optimizer$result_function
      } else {
        private$.optimizer$result_function = assert_function(rhs, null.ok = TRUE)
      }
    }
  )
)

