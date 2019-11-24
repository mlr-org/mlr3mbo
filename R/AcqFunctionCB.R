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
      super$initialize(id = "acqf_cb", settings = settings,
        opt_dir = "obj", requirements = c("response", "se"))
    },

    eval_batch = function(dt) {
      p = self$surrogate$predict_newdata(task = NULL, newdata = dt)
      res = p$mean + self$mult_max_to_min * self$settings$lambda * p$se
      data.table(acq = res, se = p$se, mean = p$mean, lambda = self$settings$lambda)
    }
  )
)
