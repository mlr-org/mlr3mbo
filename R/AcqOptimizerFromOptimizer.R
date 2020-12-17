#' @title Acquisition Optimizer from bbotk Optimizer
#'
#' @description
#' Internally used to transform [bbotk::Optimizer] to [AcqOptimizer].
#'
#' @keywords internal
#' @export
AcqOptimizerFromOptimizer = R6Class("AcqOptimizerFromOptimizer",
  inherit = AcqOptimizer,

  public = list(

    #' @field terminator [bbotk::Terminator].
    terminator = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer [bbotk::Optimizer].
    #' @param terminator [bbotk::Terminator].
    initialize = function(optimizer, terminator) {
      private$.optimizer = assert_optimizer(optimizer)
      self$terminator = assert_r6(terminator, "Terminator")

      # FIXME: handle this in the superclass
      private$.param_set = ParamSet$new(list(
        ParamDbl$new("dist_threshold", lower = 0, upper = 1))
      )
      # FIXME: adaptive default based on optimizer?
      private$.param_set$values$dist_threshold = 0
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    optimize = function(acq_function) {
      assert_r6(acq_function, "AcqFunction")

      inst = OptimInstanceSingleCrit$new(
        objective = acq_function$generate_objective(),
        search_space = acq_function$search_space, terminator = self$terminator)

      private$.optimizer$optimize(inst)
      inst$result_x_search_space
    }),

    private = list(
      .optimizer = NULL
    )
)
