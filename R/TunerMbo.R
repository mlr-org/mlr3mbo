#' @title TunerMbo
#'
#' @name mlr_tuners_mbo
#'
#' @description
#' Subclass for mbo tuning.
#'
#' @export
TunerMbo = R6Class("TunerMbo",
  inherit = mlr3tuning::TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param ... \cr
    #' Parameter passed to [OptimizerMbo].
    initialize = function(...) {
      super$initialize(
        optimizer = OptimizerMbo$new(...)
      )
    }
  )
)
