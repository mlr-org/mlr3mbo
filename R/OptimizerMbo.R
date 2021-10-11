#' @title Model Based Optimization
#'
#' @name mlr_optimizers_mbo
#'
#' @description
#' `OptimizerMbo` class that implements model based optimization.
#' The implementation follows a modular layout relying on a loop function determining the MBO flavor
#' to be used, e.g., [bayesopt_ego] for sequential single objective MBO, a [Surrogate], an
#' [AcqFunction], e.g., [AcqFunctionEI] for expected improvement and an [AcqOptimizer].
#'
#' @export
OptimizerMbo = R6Class("OptimizerMbo",
  inherit = bbotk::Optimizer,

  public = list(

    #' @field loop_function (`function` | NULL).
    loop_function = NULL,

    #' @field surrogate ([Surrogate] | NULL)
    surrogate = NULL,

    #' @field acq_function ([AcqFunction] | NULL).
    acq_function = NULL,

    #' @field acq_optimizer ([AcqOptimizer] | NULL).
    acq_optimizer = NULL,

    #' @field args (named `list()` | NULL).
    args = NULL,

    #' @field result_function (`function` | NULL).
    result_function = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `loop_function`, `surrogate`, `acq_function` and `acq_optimizer`, see `mbo_defaults`.
    #'
    #' @param loop_function (`function`)\cr
    #'   Loop function to run. See `mbo_defaults` for defaults.
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @param args (named `list()`)\cr
    #'   Further arguments passed to the `loop_function`.
    #' @param result_function (`function`)\cr
    #'   Function called after the optimization terminates.
    #'   Determines how the final result of the optimization is calculated.
    #'   Requires arguments `inst` (the [bbotk::OptimInstance]) and `self` (the [OptimizerMbo]).
    #'   See for example [result_by_surrogate_design] which is used by default if the
    #'   [bbotk::OptimInstance] has the property `"noisy"` (which is the case for a
    #'   [mlr3tuning::TuningInstanceSingleCrit] or [mlr3tuning::TuningInstanceMultiCrit])
    initialize = function(loop_function = NULL, surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, args = NULL, result_function = NULL) {
      param_set = ParamSet$new()
      param_classes = feature_types_to_param_classes(acq_function$surrogate$model$feature_types)
      properties = c("dependencies", "multi-crit", "single-crit")  # FIXME: properties should be inferred automatically
      packages = character()  # FIXME: maybe not so important? Surrogate package etc.?
      super$initialize(param_set, param_classes, properties, packages)
      self$loop_function = assert_function(loop_function, null.ok = TRUE)
      self$surrogate = assert_r6(surrogate, "Surrogate", null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, "AcqOptimizer", null.ok = TRUE)
      self$args = assert_list(args, names = "named", null.ok = TRUE)
      self$result_function = assert_function(result_function, null.ok = TRUE)
    }
  ),

  private = list(
    .optimize = function(inst) {
      # FIXME: we could log the defaults chosen?
      # FIXME: this needs some more checks for edge cases like eips
      if (is.null(self$loop_function)) {
        self$loop_function = default_loopfun(inst)
      }

      if (is.null(self$surrogate)) {
        self$surrogate = self$surrogate$acq_function %??% default_surrogate(inst)
      }

      if (is.null(self$acq_function)) {
        self$acq_function = default_acqfun(inst)
      }

      if (is.null(self$acq_optimizer)) {
        self$acq_optimizer = default_acqopt(self$acq_function)
      }

      self$surrogate$archive = inst$archive
      self$acq_function$surrogate = self$surrogate
      self$acq_optimizer$acq_function = self$acq_function

      invoke(self$loop_function, instance = inst, surrogate = self$surrogate, acq_function = self$acq_function, acq_optimizer = self$acq_optimizer, .args = self$args)
    },

    .assign_result = function(inst) {
      if (is.null(self$result_function)) {
        if ("noisy" %in% inst$objective$properties) {
          result_by_surrogate_design(inst, self)
        } else {
          super$.assign_result(inst)
        }
      } else {
        self$result_function(inst, self)  # FIXME: maybe not final API?
      }
    }
  )
)

