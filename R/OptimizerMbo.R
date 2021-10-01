#' @title OptimizerMbo
#'
#' @name mlr_optimizers_mbo
#'
#' @description
#' `OptimizerMbo` class that implements model based optimization.
#' The implementation follows a modular layout relying on a loop function` determining the MBO
#' flavor to be used, e.g., [bayesopt_soo] for sequential MBO, an acquisition function, e.g.,
#' [AcqFunctionEI] for expected improvement and an acquisition function optimizer.
#'
#' @export
OptimizerMbo = R6Class("OptimizerMbo",
  inherit = bbotk::Optimizer,

  public = list(

    #' @field loop_function (`function` | NULL).
    loop_function = NULL,

    #' @field result_function (`function` | NULL).
    result_function = NULL,

    #' @field acq_function ([AcqFunction] | NULL).
    acq_function = NULL,

    #' @field acq_optimizer ([AcqOptimizer] | NULL).
    acq_optimizer = NULL,

    #' @field args (named `list()` | NULL).
    args = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `loop_function`, `acq_function` and
    #' `acq_optimizer`, see `mbo_defaults`.
    #'
    #' @param loop_function (`function`)\cr
    #'   Loop function to run. See `mbo_defaults` for defaults.
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @param args (named `list()`)\cr
    #'   Further arguments for the `loop_function`.
    #' @param result_function (`function`)\cr
    #'   Function called after the optimization terminates.
    #'   Determines how the final result of the optimization is calculated.
    #'   Requires arguments `inst` (the [bbotk::OptimInstance]) and `self` (the [OptimizerMbo]).
    #'   See for example [result_by_surrogate_design].
    initialize = function(loop_function = NULL, acq_function = NULL, acq_optimizer = NULL, args = NULL, result_function = NULL) {
      param_set = ParamSet$new()
      param_classes = feature_types_to_param_classes(acq_function$surrogate$model$feature_types)
      properties = c("dependencies", "multi-crit", "single-crit")
      packages = character()  # FIXME: Maybe not so important? Surrogate package etc?
      super$initialize(param_set, param_classes, properties, packages)
      self$loop_function = assert_function(loop_function, null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, "AcqOptimizer", null.ok = TRUE)
      self$args = assert_list(args, names = "named", null.ok = TRUE)
      self$result_function = assert_function(result_function, null.ok = TRUE)
    }
  ),

  private = list(
    .optimize = function(inst) {
      if (is.null(self$loop_function)) {
        self$loop_function = default_loopfun(inst)
      }
      invoke(self$loop_function, instance = inst, acq_function = self$acq_function, acq_optimizer = self$acq_optimizer, .args = self$args)
    },

    .assign_result = function(inst) {
      if (is.null(self$result_function)) {
        super$.assign_result(inst)
      } else {
        self$result_function(inst, self)  # FIXME: Maybe not final API
      }
    }
  )
)

