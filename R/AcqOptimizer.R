#' @title Acquisition Optimizer
#'
#' @description
#' Optimizer for [AcqFunction] objects.
#'
#' @export
AcqOptimizer = R6Class("AcqOptimizer",
  public = list(

    optimizer = NULL,
    terminator = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(optimizer, terminator) {
      self$optimizer = assert_r6(optimizer, "Optimizer")
      self$terminator = assert_r6(terminator, "Terminator")
      ps = ParamSet$new(list(
        ParamLgl$new("fix_distance"),
        ParamDbl$new("dist_threshold", lower = 0, upper = 1))
      )

      ps$values = list(fix_distance = FALSE, dist_threshold = 0)
      ps$add_dep("dist_threshold", on = "fix_distance", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' If the `fix_distance` parameter is set to `TRUE`, proposed points are replaced by randomly
    #' sampled ones if their Gower distance with respect to other proposed points or previously
    #' evaluated points falls below the `dist_threshold` parameter.
    #'
    #' @param acq_function [AcqFunction]\cr
    #' Acquisition function to optimize.
    #' @param archive [bbotk::Archive]\cr
    #' Archive.
    #'
    #' @return [data.table::data.table()] with 1 row per optimum and x as columns.
    optimize = function(acq_function, archive) {
      if (acq_function$codomain$length == 1L) {
        instance = OptimInstanceSingleCrit$new(objective = acq_function, terminator = self$terminator)
      } else {
        if (!"multi-crit" %in% self$optimizer$properties) {
          stopf("Optimizer %s is not multi-crit compatible but %s is multi-crit.", self$self$optimizer$format(), acq_function$id)
        }
        instance = OptimInstanceMultiCrit$new(objective = acq_function, terminator = self$terminator)
      }

      xdt = tryCatch(self$optimizer$optimize(instance),
        error = function(error_condition) {
          lg$info(error_condition$message)  # FIXME: logging?
          stop(set_class(list(message = error_condition$message, call = NULL),
            classes = c("leads_to_exploration_error", "optimize_error", "error", "condition")))
        }
      )

      if (self$param_set$values$fix_distance) {
        xdt = fix_xdt_distance(xdt, previous_xdt = archive_x(archive), search_space = acq_function$domain, dist_threshold = self$param_set$values$dist_threshold)
      }

      xdt
    }
  ),

  active = list(

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
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
