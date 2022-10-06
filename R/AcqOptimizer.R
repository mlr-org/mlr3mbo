#' @title Acquisition Function Optimizer
#'
#' @description
#' Optimizer for [AcqFunction]s which performs the infill optimization.
#' Wraps an [bbotk::Optimizer] and [bbotk::Terminator].
#'
#' @section Parameters:
#' \describe{
#' \item{`logging_level`}{`character(1)`\cr
#'   Logging level during the infill optimization.
#'   Can be `"fatal"`, `"error"`, `"warn"`, `"info"`, `"debug"` or `"trace"`.
#'   Default is `"warn"`, i.e., only warnings are logged.
#' }
#' \item{`warmstart`}{`logical(1)`\cr
#'   Should the infill optimization be warm-started by evaluating the best point(s) present in the [bbotk::Archive] of
#'   the actual [bbotk::OptimInstance]?
#'   This is sensible when using a population based infill optimizer, e.g., local search or mutation.
#'   Default is `FALSE`.
#' }
#' \item{`warmstart_size`}{`integer(1) | "all"`\cr
#'   Number of best points selected from the [bbotk::Archive] that are to be used for warm starting.
#'   Can also be "all" to use all available points.
#'   Only relevant if `warmstart = TRUE`.
#'   Default is `1`.
#' }
#' \item{`skip_already_evaluated`}{`logical(1)`\cr
#'   It can happen that the candidate resulting of the infill optimization was already evaluated in a previous
#'   iteration. Should this candidate proposal be ignored and the next best point be selected as a candidate?
#'   Default is `TRUE`.
#' }
#' \item{`catch_errors`}{`logical(1)`\cr
#'   Should errors during the infill optimization be caught and propagated to the `loop_function` which can then handle
#'   the failed infill optimization appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Default is `TRUE`.
#' }
#' }
#'
#' @export
AcqOptimizer = R6Class("AcqOptimizer",
  public = list(

    #' @field optimizer ([bbotk::Optimizer]).
    optimizer = NULL,

    #' @field terminator ([bbotk::Terminator]).
    terminator = NULL,

    #' @field acq_function ([AcqFunction]).
    acq_function = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer ([bbotk::Optimizer]).
    #' @param terminator ([bbotk::Terminator]).
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(optimizer, terminator, acq_function = NULL) {
      self$optimizer = assert_r6(optimizer, "Optimizer")
      self$terminator = assert_r6(terminator, "Terminator")
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      ps = ParamSet$new(list(
        ParamFct$new("logging_level", levels = c("fatal", "error", "warn", "info", "debug", "trace"), default = "warn"),
        ParamLgl$new("warmstart", default = FALSE),
        ParamInt$new("warmstart_size", lower = 1L, special_vals = list("all")),
        ParamLgl$new("skip_already_evaluated", default = TRUE),
        ParamLgl$new("catch_errors", default = TRUE))
      )
      ps$values = list(logging_level = "warn", warmstart = FALSE, skip_already_evaluated = TRUE, catch_errors = TRUE)
      ps$add_dep("warmstart_size", on = "warmstart", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), paste0(": ", self$print_id))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values)))
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per optimum and x as columns.
    optimize = function() {
      # FIXME: currently only supports singlecrit acqfunctions
      if (self$acq_function$codomain$length > 1L) {
        stop("Multi-objective acquisition functions are currently not supported.")
      }

      logger = lgr::get_logger("bbotk")
      old_threshold = logger$threshold
      logger$set_threshold(self$param_set$values$logging_level)
      on.exit(logger$set_threshold(old_threshold))

      instance = OptimInstanceSingleCrit$new(objective = self$acq_function, search_space = self$acq_function$domain, terminator = self$terminator, check_values = FALSE, keep_evals = "all")

      # warmstart
      if (self$param_set$values$warmstart) {
        warmstart_size = if (isTRUE(self$param_set$values$warmstart_size == "all")) Inf else self$param_set$values$warmstart_size %??% 1L  # default is 1L
        n_select = min(nrow(self$acq_function$archive$data), warmstart_size)
        instance$eval_batch(self$acq_function$archive$best(n_select = n_select)[, instance$search_space$ids(), with = FALSE])
      }

      # infill optimization
      xdt = if (self$param_set$values$catch_errors) {
        tryCatch(self$optimizer$optimize(instance),
          error = function(error_condition) {
            lg$warn(error_condition$message)
            stop(set_class(list(message = error_condition$message, call = NULL),
              classes = c("acq_optimizer_error", "mbo_error", "error", "condition")))
          }
        )
      } else {
        self$optimizer$optimize(instance)
      }

      # check if candidate was already evaluated and if so get the best one not evaluated so far
      if (self$param_set$values$skip_already_evaluated) {
        if (nrow(self$acq_function$archive$data[xdt[, instance$archive$cols_x, with = FALSE], on = instance$archive$cols_x]) > 0L) {
          xdt = if (self$param_set$values$catch_errors) {
            tryCatch(get_best_not_evaluated(instance, evaluated = self$acq_function$archive$data),
              error = function(error_condition) {
                lg$warn(error_condition$message)
                stop(set_class(list(message = error_condition$message, call = NULL),
                  classes = c("acq_optimizer_error", "mbo_error", "error", "condition")))
              }
            )
          } else {
            get_best_not_evaluated(instance, evaluated = self$acq_function$archive$data)
          }
        }
      }

      xdt
    }
  ),

  active = list(

    #' @field print_id (`character`)\cr
    #' Id used when printing.
    print_id = function(rhs) {
      if (missing(rhs)) {
        paste0("(", class(self$optimizer)[1L], " | ", class(self$terminator)[1L], ")")
      } else {
        stop("'print_id' field is read-only.")
      }
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(

    .param_set = NULL,

    deep_clone = function(name, value) {
      switch(name,
        optimizer = value$clone(deep = TRUE),
        terminator = value$clone(deep = TRUE),
        acq_function = if (!is.null(value)) value$clone(deep = TRUE) else NULL,
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
