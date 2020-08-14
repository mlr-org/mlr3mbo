#' @title Acquisition Function Confidence Bound
#'
#' @description
#'  Confidence Bound.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`}.
#' }
#'
#' @export
AcqFunctionCB = R6Class("AcqFunctionCB",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      param_set = ParamSet$new(list(
        ParamDbl$new("lambda", lower = 0, default = 2)
      ))
      param_set$values$lambda = 2
      assert_r6(surrogate, "SurrogateSingleCrit")
      super$initialize("acq_cb", param_set, surrogate, direction = "same")
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table]
    #'
    #' @return `data.table`
    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      res = p$mean - self$surrogate_max_to_min * self$param_set$values$lambda * p$se
      # FIXME: what do we return here? do we want to see se, mean, too?
      data.table(acq_cb = res)
    }
  )
)
