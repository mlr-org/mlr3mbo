#' @title TunerMbo
#'
#' @name mlr_tuners_mbo
#'
#' @description
#' Subclass for mbo tuning.
#'
#' @inheritSection OptimizerMbo Parameters
#'
#' @export
#' @template example
TunerMbo = R6Class("TunerMbo",
  inherit = mlr3tuning::TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(
        optimizer = OptimizerMbo$new(...)
      )
    }
  )
)

mlr_tuners$add("mbo", TunerMbo)
