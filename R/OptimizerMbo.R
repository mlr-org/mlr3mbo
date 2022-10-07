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
#' For more information on default values for `loop_function`, `surrogate`, `acq_function` and `acq_optimizer`, see `?mbo_defaults`.
#'
#' @export
OptimizerMbo = R6Class("OptimizerMbo",
  inherit = bbotk::Optimizer,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' For more information on default values for `loop_function`, `surrogate`, `acq_function` and `acq_optimizer`, see `?mbo_defaults`.
    #'
    #' @template param_loop_function
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @template param_args
    #' @template param_result_function
    initialize = function(loop_function = NULL, surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, args = NULL, result_function = NULL) {
      param_set = ParamSet$new()
      super$initialize("mbo",
                       param_set = param_set,
                       param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),  # is replaced with dynamic AB after construction
                       properties = c("dependencies", "multi-crit", "single-crit"),  # is replaced with dynamic AB after construction
                       packages = "mlr3mbo",  # is replaced with dynamic AB after construction
                       label = "Model Based Optimization",
                       man = "mlr3mbo::OptimizerMbo")
      self$loop_function = assert_loop_function(loop_function)
      self$surrogate = assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
      assert_list(args, names = "named", null.ok = TRUE)
      if (!is.null(self$loop_function)) {
        assert_subset(names(args), choices = setdiff(names(formals(self$loop_function)), c("instance", "surrogate", "acq_function", "acq_optimizer")), empty.ok = TRUE)
      }
      self$args = args
      self$result_function = assert_function(result_function, args = c("instance", "optimizer_mbo"), null.ok = TRUE)
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
    }
  ),

  active = list(
    #' @template field_loop_function
    loop_function = function(rhs) {
      if (missing(rhs)) {
        private$.loop_function
      } else {
        private$.loop_function = assert_loop_function(rhs)
      }
    },

    #' @template field_surrogate
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        private$.surrogate = assert_r6(rhs, classes = "Surrogate", null.ok = TRUE)
      }
    },

    #' @template field_acq_function
    acq_function = function(rhs) {
      if (missing(rhs)) {
        private$.acq_function
      } else {
        private$.acq_function = assert_r6(rhs, classes = "AcqFunction", null.ok = TRUE)
      }
    },

    #' @template field_acq_optimizer
    acq_optimizer = function(rhs) {
      if (missing(rhs)) {
        private$.acq_optimizer
      } else {
        private$.acq_optimizer = assert_r6(rhs, classes = "AcqOptimizer", null.ok = TRUE)
      }
    },

    #' @template field_args
    args = function(rhs) {
      if (missing(rhs)) {
        if (!is.null(self$loop_function)) {
          assert_subset(names(private$.args), choices = setdiff(names(formals(self$loop_function)), c("instance", "surrogate", "acq_function", "acq_optimizer")), empty.ok = TRUE)  # args could have been set prior to a loop_function
        }
        private$.args
      } else {
        assert_list(rhs, names = "named", null.ok = TRUE)
        if (!is.null(self$loop_function)) {
          assert_subset(names(rhs), choices = setdiff(names(formals(self$loop_function)), c("instance", "surrogate", "acq_function", "acq_optimizer")), empty.ok = TRUE)
        }
        private$.args = rhs
      }
    },

    #' @template field_result_function
    result_function = function(rhs) {
      if (missing(rhs)) {
        private$.result_function
      } else {
        private$.result_function = assert_function(rhs, args = c("instance", "optimizer_mbo"), null.ok = TRUE)
      }
    },

    #' @template field_param_classes
    param_classes = function(rhs) {
      if (missing(rhs)) {
        param_classes_surrogate = c("logical" = "ParamLgl", "integer" = "ParamInt", "numeric" = "ParamDbl", "factor" = "ParamFct")
        if (!is.null(self$surrogate)) {
          param_classes_surrogate = param_classes_surrogate[c("logical", "integer", "numeric", "factor") %in% self$surrogate$feature_types] # surrogate has prio before acq_function$surrogate
        }
        param_classes_acq_opt = if (!is.null(self$acq_optimizer)) {
          self$acq_optimizer$optimizer$param_classes
        } else {
          c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct")
        }
        unname(intersect(param_classes_surrogate, param_classes_acq_opt))
      } else {
        stop("param_classes is read-only.")
      }
    },

    #' @template field_properties
    properties = function(rhs) {
      if (missing(rhs)) {
        properties_loop_function = c("single-crit", "multi-crit")
        if (!is.null(self$loop_function)) {
          properties_loop_function = intersect(properties_loop_function, attr(self$loop_function, "instance"))
        }
        properties_surrogate = "dependencies"
        if (!is.null(self$surrogate)) {
          if ("missings" %nin% self$surrogate$properties) {
            properties_surrogate = character()
          }
        }
        unname(c(properties_surrogate, properties_loop_function))
      } else {
        stop("properties is read-only.")
      }
    },

    #' @template field_packages
    packages = function(rhs) {
      if (missing(rhs)) {
        union("mlr3mbo", c(self$surrogate$packages, self$acq_optimizer$optimizer$packages))
      } else {
        stop("$packages is read-only.")
      }
    }
  ),

  private = list(
    .loop_function = NULL,
    .surrogate = NULL,
    .acq_function = NULL,
    .acq_optimizer = NULL,
    .result_function = NULL,
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

