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
    initialize = function() {
      super$initialize(
        id = "acqf_mean",
        settings = list(),
        opt_dir = "obj",
        requirements = "response"
      )
    },

    eval_batch = function(dt) {
      p = self$surrogate$predict_newdata(dt)
      p$response
    }
  )
)

