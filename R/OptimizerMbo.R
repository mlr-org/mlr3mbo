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

    #' @template field_loop_function
    loop_function = NULL,

    #' @template field_surrogate
    surrogate = NULL,

    #' @template field_acq_function
    acq_function = NULL,

    #' @template field_acq_optimizer
    acq_optimizer = NULL,

    #' @template field_result_function
    result_function = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `loop_function`, `surrogate`, `acq_function` and `acq_optimizer`, see `mbo_defaults`.
    #'
    #' @template param_loop_function
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @template param_args
    #' @template param_result_function
    initialize = function(loop_function = NULL, surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, args = NULL, result_function = NULL) {
      param_set = ParamSet$new()
      param_classes = feature_types_to_param_classes(acq_function$surrogate$model$feature_types)  # FIXME: delayed initialization?
      properties = c("dependencies", "multi-crit", "single-crit")  # FIXME: properties should be inferred automatically
      super$initialize("mbo", param_set, param_classes, properties, c("mlr3mbo"), label = "Model Based Optimization", man = "mlr3mbo::OptimizerMbo")
      self$loop_function = assert_function(loop_function, null.ok = TRUE)
      self$surrogate = assert_r6(surrogate, "Surrogate", null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, "AcqOptimizer", null.ok = TRUE)
      self$args = assert_list(args, names = "named", null.ok = TRUE)
      self$result_function = assert_function(result_function, null.ok = TRUE)
    }
  ),

  active = list(
    #' @template field_args
    args = function(rhs) {
      if (missing(rhs)) {
        private$.args
      } else {
        assert_list(rhs, names = "named", null.ok = TRUE)
        if (!is.null(self$loop_function)) {
          assert_subset(rhs, choices = formals(self$loop_function), empty.ok = TRUE)
        }
        private$.args = rhs
      }
    },

    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled prior to optimization if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
    #'   Required packages are determined based on the `surrogate` and the `acq_optimizer`.
    packages = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.packages)) {
        stop("$packages is read-only.")
      } else {
        union("mlr3mbo", c(self$surrogate$packages, self$acq_optimizer$optimizer$packages))
      }
    }

  ),

  private = list(
    .args = NULL,

    .optimize = function(inst) {
      # FIXME: this needs some more checks for edge cases like eips
      if (is.null(self$loop_function)) {
        self$loop_function = default_loopfun(inst)
      }

      if (is.null(self$surrogate)) {
        self$surrogate = self$acq_function$surrogate %??% default_surrogate(inst)
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

      check_packages_installed(self$packages, msg = sprintf("Package '%%s' required but not installed for Optimizer '%s'", format(self)))

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
        self$result_function(inst, self)
      }
    }
  )
)

