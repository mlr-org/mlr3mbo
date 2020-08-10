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
AcqFunctionMean = R6Class("AcqFunctionMean", inherit = AcqFunction,
  public = list(
    # FIXME: should we add some metainfo fequiremements what the acqf needs? eg se?

    initialize = function(surrogate) {
      super$initialize(
        id = "acq_mean",
        param_set = ParamSet$new(),
        surrogate = surrogate
      )
    },

    eval_dt = function(xdt) {
      p = self$surrogate$predict_newdata(dt)
      data.table(acq_mean = p$response)
    }
  )
)

