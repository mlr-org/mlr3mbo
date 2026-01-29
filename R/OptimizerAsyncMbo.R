#' @title Asynchronous Model Based Optimization
#'
#' @name mlr_optimizers_async_mbo
#'
#' @description
#' `OptimizerAsyncMbo` class that implements Asynchronous Model Based Optimization (AMBO).
#' AMBO starts multiple sequential MBO runs on different workers.
#' The worker communicate asynchronously through a shared archive relying on the \CRANpkg{rush} package.
#' The optimizer follows a modular layout in which the surrogate model, acquisition function, and acquisition optimizer can be changed.
#' The [SurrogateLearner] will impute missing values due to pending evaluations.
#' A stochastic [AcqFunction], e.g., [AcqFunctionStochasticEI] or [AcqFunctionStochasticCB] is used to create varying versions of the acquisition
#' function on each worker, promoting different exploration-exploitation trade-offs.
#' The [AcqOptimizer] class remains consistent with the one used in synchronous MBO.
#'
#' In contrast to [OptimizerMbo], no [loop_function] can be specified that determines the AMBO flavor as `OptimizerAsyncMbo` simply relies on
#' a surrogate update, acquisition function update and acquisition function optimization step as an internal loop.
#'
#' Currently, only single-objective optimization is supported and `OptimizerAsyncMbo` is considered an experimental feature and API might be subject to changes.
#'
#' Note that in general the [SurrogateLearner] is updated one final time on all available data after the optimization process has terminated.
#' However, in certain scenarios this is not always possible or meaningful.
#' It is therefore recommended to manually inspect the [SurrogateLearner] after optimization if it is to be used, e.g., for visualization purposes to make
#' sure that it has been properly updated on all available data.
#' If this final update of the [SurrogateLearner] could not be performed successfully, a warning will be logged.
#'
#' By specifying a [ResultAssigner], one can alter how the final result is determined after optimization, e.g.,
#' simply based on the evaluations logged in the archive [ResultAssignerArchive] or based on the [Surrogate] via [ResultAssignerSurrogate].
#'
#' @section Archive:
#' The [bbotk::ArchiveAsync] holds the following additional columns that are specific to AMBO algorithms:
#'   * `acq_function$id` (`numeric(1)`)\cr
#'     The value of the acquisition function.
#'   * `".already_evaluated"` (`logical(1))`\cr
#'     Whether this point was already evaluated. Depends on the `skip_already_evaluated` parameter of the [AcqOptimizer].
#'
#' If the [bbotk::ArchiveAsync] does not contain any evaluations prior to optimization, an initial design is needed.
#' If the `initial_design` parameter is specified to be a `data.table`, this data will be used.
#' Otherwise, if it is `NULL`, an initial design of size `design_size` will be generated based on the `generate_design` sampling function.
#' See also the parameters below.
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
#'     library(bbotk)
#'     library(paradox)
#'     library(mlr3learners)
#'
#'     fun = function(xs) {
#'       list(y = xs$x ^ 2)
#'     }
#'     domain = ps(x = p_dbl(lower = -10, upper = 10))
#'     codomain = ps(y = p_dbl(tags = "minimize"))
#'     objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'     instance = OptimInstanceAsyncSingleCrit$new(
#'       objective = objective,
#'       terminator = trm("evals", n_evals = 10))
#'
#'     mirai::daemons(2)
#'     rush::rush_plan(n_workers=2, worker_type = "remote")
#'
#'     optimizer = opt("async_mbo", design_size = 4, n_workers = 2)
#'
#'     optimizer$optimize(instance)
##'     mirai::daemons(0)
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
OptimizerAsyncMbo = R6Class("OptimizerAsyncMbo",
  inherit = bbotk::OptimizerAsync,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' If `surrogate` is `NULL` and the `acq_function$surrogate` field is populated, this [SurrogateLearner] is used.
    #' Otherwise, `default_surrogate(instance)` is used.
    #' If `acq_function` is `NULL` and the `acq_optimizer$acq_function` field is populated, this [AcqFunction] is used (and therefore its `$surrogate` if populated; see above).
    #' Otherwise `default_acqfunction(instance)` is used.
    #' If `acq_optimizer` is `NULL`, `default_acqoptimizer(instance)` is used.
    #'
    #' Even if already initialized, the `surrogate$archive` field will always be overwritten by the [bbotk::ArchiveAsync] of the current [bbotk::OptimInstanceAsyncSingleCrit] to be optimized.
    #'
    #' For more information on default values for `surrogate`, `acq_function`, `acq_optimizer` and `result_assigner`, see `?mbo_defaults`.
    #'
    #' @template param_id
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @template param_result_assigner
    #' @template param_label
    #' @param param_set ([paradox::ParamSet])\cr
    #'  Set of control parameters.
    #' @template param_man
    initialize = function(
      id = "async_mbo",
      surrogate = NULL,
      acq_function = NULL,
      acq_optimizer = NULL,
      result_assigner = NULL,
      param_set = NULL,
      label = "Asynchronous Model Based Optimization",
      man = "mlr3mbo::OptimizerAsyncMbo"
      ) {

      default_param_set = ps(
        initial_design = p_uty(),
        design_size = p_int(lower = 1, default = 100L),
        design_function = p_fct(c("random", "sobol", "lhs"), default = "sobol"),
        n_workers = p_int(lower = 1L)
      )
      param_set = c(default_param_set, param_set)

      param_set$set_values(design_size = 100L, design_function = "sobol")

      super$initialize("async_mbo",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),  # is replaced with dynamic AB after construction
        properties = c("dependencies", "single-crit"),  # is replaced with dynamic AB after construction
        packages = c("mlr3mbo", "rush"),  # is replaced with dynamic AB after construction
        label = label,
        man = man)

      self$surrogate = assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
      self$result_assigner = assert_r6(result_assigner, classes = "ResultAssigner", null.ok = TRUE)
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
    #' Reset the optimizer.
    #' Sets the following fields to `NULL`:
    #' `surrogate`, `acq_function`, `acq_optimizer`,`result_assigner`
    #' Resets parameter values `design_size` and `design_function` to their defaults.
    reset = function() {
      private$.surrogate = NULL
      private$.acq_function = NULL
      private$.acq_optimizer = NULL
      private$.result_assigner = NULL
      self$param_set$set_values(design_size = 100L, design_function = "sobol")
    },

    #' @description
    #' Performs the optimization on an [bbotk::OptimInstanceAsyncSingleCrit] until termination.
    #' The single evaluations will be written into the [bbotk::ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([bbotk::OptimInstanceAsyncSingleCrit]).
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      if (is.null(self$acq_function)) {
        self$acq_function = self$acq_optimizer$acq_function %??% default_acqfunction(inst)
      }

      if (is.null(self$surrogate)) {  # acq_function$surrogate has precedence
        self$surrogate = self$acq_function$surrogate %??% default_surrogate(inst)
      }

      if (is.null(self$acq_optimizer)) {
        self$acq_optimizer = default_acqoptimizer(self$acq_function, inst)
      }

      if (is.null(self$result_assigner)) {
        self$result_assigner = default_result_assigner(inst)
      }

      self$surrogate$reset()
      self$acq_function$reset()
      self$acq_optimizer$reset()

      self$surrogate$archive = inst$archive
      self$acq_function$surrogate = self$surrogate
      self$acq_optimizer$acq_function = self$acq_function

      # FIXME: if result_assigner is for example ResultAssignerSurrogate the surrogate won't be set automatically

      check_packages_installed(self$packages, msg = sprintf("Package '%%s' required but not installed for Optimizer '%s'", format(self)))

      lg = lgr::get_logger("mlr3/bbotk")
      pv = self$param_set$values

      # initial design
      design = if (inst$archive$n_evals) {
        lg$debug("Using archive with %s evaluations as initial design", inst$archive$n_evals)
        NULL
      } else if (is.null(pv$initial_design)) {
        # generate initial design
        generate_design = switch(pv$design_function,
          "random" = generate_design_random,
          "sobol" = generate_design_sobol,
          "lhs" = generate_design_lhs)

        lg$debug("Generating sobol design with size %s", pv$design_size)
        generate_design(inst$search_space, n = pv$design_size)$data
      } else {
        # use provided initial design
        lg$info("Using provided initial design with size %s", nrow(pv$initial_design))

        xss = transpose_list(pv$initial_design[, inst$archive$cols_x, with = FALSE])
        yss = transpose_list(pv$initial_design[, inst$archive$cols_y, with = FALSE])
        inst$archive$push_finished_points(xss, yss)
        NULL
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
        private$.surrogate = assert_r6(rhs, classes = "SurrogateLearner", null.ok = TRUE)
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

    #' @template field_result_assigner
    result_assigner = function(rhs) {
      if (missing(rhs)) {
        private$.result_assigner
      } else {
        private$.result_assigner = assert_r6(rhs, classes = "ResultAssigner", null.ok = TRUE)
      }
    },

    #' @template field_param_classes
    param_classes = function(rhs) {
      if (missing(rhs)) {
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
      } else {
        stop("$param_classes is read-only.")
      }
    },

    #' @template field_properties
    properties = function(rhs) {
      if (missing(rhs)) {
        properties_loop_function = "single-crit"
        properties_surrogate = "dependencies"
        if (!is.null(self$surrogate)) {
          if ("missings" %nin% self$surrogate$properties) {
            properties_surrogate = character()
          }
        }
        unname(c(properties_surrogate, properties_loop_function))
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_packages
    packages = function(rhs) {
      if (missing(rhs)) {
        union(c("mlr3mbo", "rush"), c(self$acq_function$packages, self$surrogate$packages, self$acq_optimizer$optimizer$packages, self$result_assigner$packages))
      } else {
        stop("$packages is read-only.")
      }
    }
  ),

  private = list(
    .surrogate = NULL,
    .acq_function = NULL,
    .acq_optimizer = NULL,
    .result_assigner = NULL,

    .optimize = function(inst) {
      lg = lgr::get_logger("mlr3/bbotk")
      lg$debug("Optimizer '%s' evaluates the initial design", self$id)
      get_private(inst)$.eval_queue()

      lg$debug("Optimizer '%s' starts the optimization phase", self$id)

      # actual loop
      while (!inst$is_terminated) {
        # sample
        xs = tryCatch({
          timestamp_surrogate = Sys.time()
          self$acq_function$surrogate$update()
          timestamp_acq_function = Sys.time()
          self$acq_function$update()
          timestamp_acq_optimizer = Sys.time()
          xdt = self$acq_optimizer$optimize()
          xs = transpose_list(xdt)[[1L]]
          timestamp_loop = Sys.time()
          c(xs, list(
            timestamp_surrogate = timestamp_surrogate,
            timestamp_acq_function = timestamp_acq_function,
            timestamp_acq_optimizer = timestamp_acq_optimizer,
            timestamp_loop = timestamp_loop
          ))
        }, mbo_error = function(mbo_error_condition) {
          lg$info(paste0(class(mbo_error_condition), collapse = " / "))
          lg$info("Proposing a randomly sampled point")
          xdt = generate_design_random(inst$search_space, n = 1L)$data
          xs = transpose_list(xdt)[[1L]]
          c(xs, list(
            timestamp_surrogate = NA,
            timestamp_acq_function = NA,
            timestamp_acq_optimizer = NA,
            timestamp_loop = NA
          ))
        })

        # eval
        get_private(inst)$.eval_point(xs)
      }

      on.exit({
        tryCatch(
          {
            self$surrogate$update()
          }, surrogate_update_error = function(error_condition) {
            lg$warn("Could not update the surrogate a final time after the optimization process has terminated.")
          }
        )
      })
    },

    .assign_result = function(inst) {
      self$result_assigner$assign_result(inst)
    }
  )
)

#' @include aaa.R
optimizers[["async_mbo"]] = OptimizerAsyncMbo
