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
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    #' @return `data.table` with 1 row per optimum and x as colums
    optimize = function(acq_function) {
      if (acq_function$codomain$length == 1) {
        instance = OptimInstanceSingleCrit$new(objective = acq_function, terminator = self$terminator, check_values = FALSE, keep_evals = "best")
      } else {
        if (!"multi-crit" %in% self$optimizer$properties) {
          stopf("Optimizer %s is not multi-crit compatible but %s is multi-crit.", self$self$optimizer$format(), acq_function$id)
        }
        instance = OptimInstanceMultiCrit$new(objective = acq_function, terminator = self$terminator, check_values = FALSE, keep_evals = "best")
      }
      self$optimizer$optimize(instance)
    }
  )
)
