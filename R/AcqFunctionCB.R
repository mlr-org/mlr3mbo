#' @title Acquisition function: Confidence Bound
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' @section Fields: See [AcqFunction]
#' @section Methods: See [AcqFunction]
#' @export
AcqFunctionCB = R6Class( "AcqFunctionCB", inherit = AcqFunction,
  public = list(
    initialize = function(learner, lambda = 2) {
      settings = list(lambda = assert_number(lambda, lower = 0))
      fun = function(dt) {
        p = self$surrogate$predict_newdata(task = NULL, newdata = dt)
        res = p$mean + self$mult_max_to_min * self$settings$lambda * p$se
        # FIXME: what do we return here? do we want to see se, mean, too?
        data.table(y = res)
      }
      super$initialize(
        id = "acqf_cb", 
        fun = fun, 
        settings = settings,
        objective = objective,
        minimize = objective$minimize
      )
    }
  )
)
