#' @title Asynchronous Model Based Optimization
#' @name mlr_optimizers_async_mbo
#'
#' @description
#' `OptimizerAsyncMbo` class that implements asynchronous Model Based Optimization (MBO).
#'
#' @export
OptimizerAsyncMbo = R6Class("OptimizerAsyncMbo",
  inherit = OptimizerAsync,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(surrogate = NULL, acq_function = NULL, acq_optimizer = NULL) {
      super$initialize("async_mbo",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit"),
        packages = c("mlr3mbo", "rush"),
        label = "Asynchronous Model Based Optimization",
        man = "mlr3mbo::OptimizerAsyncMbo")

      self$surrogate = assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
    },


    #' @description
    #' Performs the optimization on a [OptimInstanceAsyncSingleCrit] or [OptimInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([OptimInstanceAsyncSingleCrit] | [OptimInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      pv = self$param_set$values

      # initial design
      design = if (is.null(pv$initial_design)) {

        lg$debug("Generating sobol design with size %s", pv$design_size)
        generate_design_sobol(inst$search_space, n = pv$design_size)$data
      } else {

        lg$debug("Using provided initial design with size %s", nrow(pv$initial_design))
        pv$initial_design
      }

      optimize_async_default(inst, self, design, n_workers = pv$n_workers)
    }
  ),

  active = list(
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

    #' @template field_param_classes
    param_classes = function(rhs) {
      assert_ro_binding(rhs)
      param_classes_surrogate = c("logical" = "ParamLgl", "integer" = "ParamInt", "numeric" = "ParamDbl", "factor" = "ParamFct")
      if (!is.null(self$surrogate)) {
        param_classes_surrogate = param_classes_surrogate[c("logical", "integer", "numeric", "factor") %in% self$surrogate$feature_types] # surrogate has precedence over acq_function$surrogate
      }
      param_classes_acq_opt = if (!is.null(self$acq_optimizer)) {
        self$acq_optimizer$optimizer$param_classes
      } else {
        c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct")
      }
      unname(intersect(param_classes_surrogate, param_classes_acq_opt))
    },

    #' @template field_properties
    properties = function(rhs) {
      assert_ro_binding(rhs)

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
    },

    #' @template field_packages
    packages = function(rhs) {
      assert_ro_binding(rhs)
      union("mlr3mbo", c(self$acq_function$packages, self$surrogate$packages, self$acq_optimizer$optimizer$packages, self$result_assigner$packages))
    }
  ),

  private = list(
    .surrogate = NULL,
    .acq_function = NULL,
    .acq_optimizer = NULL,

    .optimize = function(inst) {
      pv = self$param_set$values
      search_space = inst$search_space
      archive = inst$archive

      if (is.null(self$acq_function)) {
        self$acq_function = self$acq_optimizer$acq_function %??% default_acqfunction(inst)
      }

      if (is.null(self$surrogate)) {
        self$surrogate = self$acq_function$surrogate %??% default_surrogate(inst)
      }

      if (is.null(self$acq_optimizer)) {
        self$acq_optimizer = default_acqoptimizer(self$acq_function)
      }

      surrogate$archive = inst$archive
      acq_function$surrogate = surrogate
      acq_optimizer$acq_function = acq_function

      lg$debug("Optimizer '%s' evaluates the initial design", self$id)
      evaluate_queue_default(inst)

      lg$debug("Optimizer '%s' starts the tuning phase", self$id)

      # actual loop
      while (!inst$is_terminated) {
        # sample
        acq_function$surrogate$update()
        acq_function$update()
        xdt = acq_optimizer$optimize()

        # transpose point
        xss = transpose_list(xdt)
        xs = xss[[1]][inst$archive$cols_x]
        lg$trace("Optimizer '%s' draws %s", self$id, as_short_string(xs))
        xs_trafoed = trafo_xs(xs, search_space)

        # eval
        key = archive$push_running_point(xs)
        ys = inst$objective$eval(xs_trafoed)

        # push result
        extra = c(xss[[1]][c("acq_cb", ".already_evaluated")], list(lambda_0 = lambda_0, lambda = lambda))
        archive$push_result(key, ys, x_domain = xs_trafoed, extra = extra)
      }
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
