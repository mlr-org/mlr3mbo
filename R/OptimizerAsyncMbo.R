#' @title Asynchronous Model Based Optimization
#'
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
    initialize = function(surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, param_set = NULL) {
      default_param_set = ps(
        initial_design = p_uty(),
        design_size = p_int(lower = 1, default = 10),
        design_function = p_fct(c("random", "sobol", "lhs"), default = "sobol"),
        n_workers = p_int(lower = 1L, default = NULL, special_vals = list(NULL))
      )
      param_set = c(default_param_set, param_set)

      param_set$set_values(design_size = 10, design_function = "sobol")

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
        # generate initial design
        generate_design = switch(pv$design_function,
          "random" = generate_design_random,
          "sobol" = generate_design_sobol,
          "lhs" = generate_design_lhs)

        lg$debug("Generating sobol design with size %s", pv$design_size)
        generate_design(inst$search_space, n = pv$design_size)$data
      } else {
        # use provided initial design
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

      properties_surrogate = "dependencies"
      if (!is.null(self$surrogate)) {
        if ("missings" %nin% self$surrogate$properties) {
          properties_surrogate = character()
        }
      }
      unname(c(properties_surrogate))
    },

    #' @template field_packages
    packages = function(rhs) {
      assert_ro_binding(rhs)
      union("mlr3mbo", c(self$acq_function$packages, self$surrogate$packages, self$acq_optimizer$optimizer$packages))
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

      self$surrogate$archive = inst$archive
      self$acq_function$surrogate = self$surrogate
      self$acq_optimizer$acq_function = self$acq_function

      lg$debug("Optimizer '%s' evaluates the initial design", self$id)
      get_private(inst)$.eval_queue()

      lg$debug("Optimizer '%s' starts the tuning phase", self$id)

      # actual loop
      while (!inst$is_terminated) {
        # sample
        self$acq_function$surrogate$update()
        self$acq_function$update()
        xdt = self$acq_optimizer$optimize()
        xs = transpose_list(xdt)[[1]]

        # eval
        get_private(inst)$.eval_point(xs)
      }
    }
  )
)

#' @include aaa.R
optimizers[["async_mbo"]] = OptimizerAsyncMbo
