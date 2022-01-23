#' @title Tuner using Bayesian Optimization
#'
#' @name mlr_tuners_mbo
#'
#' @description
#' Tune hyperparameters using Bayesian Optimization.
#' This is a minimal interface internally passing on to [OptimizerMbo].
#' For additional information and documentation see [OptimizerMbo].
#'
#' @export
TunerMbo = R6Class("TunerMbo",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param ... Parameters passed to [mlr3mbo::OptimizerMbo].
    initialize = function(...) {
      super$initialize(optimizer = OptimizerMbo$new(...))
    }
  )
)

