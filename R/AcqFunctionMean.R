#' @title Acquisition function: Posterior Mean
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' @section Fields: See [AcqFunction]
#' @section Methods: See [AcqFunction]
#' @export
AcqFunctionMean = R6Class( "AcqFunctionMean", inherit = AcqFunction,
  public = list(
    # FIXME: should we add some metainfo fequiremements what the acqf needs? eg se?

    initialize = function(objective) {
      fun = function(dt) {
        p = self$surrogate$predict_newdata(dt)
        data.table(y = p$response)
      }
      super$initialize(
        id = "acqf_mean",
        fun = fun, 
        objective = objective,
        minimize = objective$minimize
      )
    }
  )
)

