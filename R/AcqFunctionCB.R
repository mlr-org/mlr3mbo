#' @title Acquisition function: Confidence Bound
#'
#' @export
AcqFunctionCB = R6Class("AcqFunctionCB",
  inherit = AcqFunction,
  public = list(
    initialize = function(surrogate) {
      param_set = ParamSet$new(list(
        ParamDbl$new("lambda", lower = 0, default = 2)
      ))
      param_set$values$lambda = 2

      super$initialize("acq_cb", param_set, surrogate, direction = "same")
    },

    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      res = p$mean - self$mult_max_to_min * self$param_set$values$lambda * p$se
      # FIXME: what do we return here? do we want to see se, mean, too?
      data.table(acq_cb = res)
    }
  )
)
