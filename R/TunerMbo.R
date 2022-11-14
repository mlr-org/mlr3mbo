#' @title Tuner using Model Based Optimization
#'
#' @name mlr_tuners_mbo
#'
#' @description
#' `TunerMbo` class that implements Model Based Optimization (MBO).
#' This is a minimal interface internally passing on to [OptimizerMbo].
#' For additional information and documentation see [OptimizerMbo].
#'
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'
#'   library(mlr3)
#'   library(mlr3tuning)
#'
#'   # single-objective
#'   task = tsk("wine")
#'   learner = lrn("classif.rpart", cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE))
#'   resampling = rsmp("cv", folds = 3)
#'   measure = msr("classif.acc")
#'
#'   instance = TuningInstanceSingleCrit$new(
#'     task = task,
#'     learner = learner,
#'     resampling = resampling,
#'     measure = measure,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   tnr("mbo")$optimize(instance)
#'
#'   # multi-objective
#'   task = tsk("wine")
#'   learner = lrn("classif.rpart", cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE))
#'   resampling = rsmp("cv", folds = 3)
#'   measures = msrs(c("classif.acc", "selected_features"))
#'
#'   instance = TuningInstanceMultiCrit$new(
#'     task = task,
#'     learner = learner,
#'     resampling = resampling,
#'     measures = measures,
#'     terminator = trm("evals", n_evals = 5),
#'     store_models = TRUE) # required due to selected features
#'
#'   tnr("mbo")$optimize(instance)
#' }
#' }
TunerMbo = R6Class("TunerMbo",
  inherit = mlr3tuning::TunerFromOptimizer,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `loop_function`, `surrogate`, `acq_function` and `acq_optimizer`, see `?mbo_defaults`.
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
      catn(str_indent("* Loop function:", if (is.null(self$loop_function)) "-" else attr(self$loop_function, "id")))
      catn(str_indent("* Surrogate:", if (is.null(self$surrogate)) "-" else self$surrogate$print_id))
      catn(str_indent("* Acquisition Function:", if (is.null(self$acq_function)) "-" else class(self$acq_function)[1L]))
      catn(str_indent("* Acquisition Function Optimizer:", if (is.null(self$acq_optimizer)) "-" else self$acq_optimizer$print_id))
    },

    #' @description
    #' Reset the tuner.
    #' Sets the following fields to `NULL`:
    #' `loop_function`, `surrogate`, `acq_function`, `acq_optimizer`, `args`, `result_function`
    reset = function() {
      private$.optimizer$reset()
    }
  ),

  active = list(
    #' @template field_loop_function
    loop_function = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$loop_function
      } else {
        private$.optimizer$loop_function = assert_loop_function(rhs)
      }
    },

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

    #' @template field_args
    args = function(rhs) {
      if (missing(rhs)) {
       if (!is.null(private$.optimizer$loop_function)) {
          assert_subset(names(private$.args), choices = setdiff(names(formals(private$.optimizer$loop_function)), c("instance", "surrogate", "acq_function", "acq_optimizer")), empty.ok = TRUE)  # args could have been set prior to a loop_function
        }
        private$.optimizer$args
      } else {
        assert_list(rhs, names = "named", null.ok = TRUE)
        if (!is.null(private$.optimizer$loop_function)) {
          assert_subset(names(rhs), choices = setdiff(names(formals(private$.optimizer$loop_function)), c("instance", "surrogate", "acq_function", "acq_optimizer")), empty.ok = TRUE)
        }
        private$.optimizer$args = rhs
      }
    },

    #' @template field_result_function
    result_function = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$result_function
      } else {
        private$.optimizer$result_function = assert_function(rhs, args = c("instance", "optimizer_mbo"), null.ok = TRUE)
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

