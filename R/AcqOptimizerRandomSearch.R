#' @title Acquisition Optimizer Random Search
#'
#' @description
#' `AcqOptimizerRandomSearch` class that implements a random search for the
#' optimization of acquisition functions.
#'
#' @export
AcqOptimizerRandomSearch = R6Class("AcqOptimizerRandomSearch",
  inherit = AcqOptimizer,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      private$.param_set = ParamSet$new(list(
        ParamInt$new("iters", lower = 1L, default = 1000L),
        ParamDbl$new("dist_threshold", lower = 0, upper = 1))  # FIXME: handle this in the superclass
      )
      # FIXME: adaptive default based on optimizer?
      private$.param_set$values$dist_threshold = 0
      private$.param_set$values$iters = 1000L
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    optimize = function(acq_function) {
      xdt = generate_design_random(acq_function$search_space, self$param_set$values$iters)$data
      ydt = acq_function$eval_dt(xdt) * mult_max_to_min(acq_function$codomain)
      best = which(ydt[[1]] == min(ydt[[1]]))
      if (length(best) > 1) {
        best = sample(best, 1)
      }
      xdt[best,]
    }
))
