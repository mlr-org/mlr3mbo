#' @title Acquisition function: Expected Improvement
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' @section Fields: See [AcqFunction]
#' @section Methods: See [AcqFunction]
#' @export
AcqFunctionEI = R6Class("AcqFunctionEI",
  inherit = AcqFunction,
  public = list(

    y_best = NULL, 

    initialize = function(surrogate) {
      param_set = ParamSet$new()
      super$initialize("acq_ei", param_set, surrogate)
    },

    setup = function(archive) {
      super$setup(archive, direction = "max")
      y_best = archive$best()[, archive$cols$y, with = FALSE]
    },

    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      res = p$mean + self$mult_max_to_min * self$settings$lambda * p$se
      # FIXME: what do we return here? do we want to see se, mean, too?
      data.table(acq_cb = res)
    },

    update = function(archive) {
      y_best = archive$best()[, archive$cols$y, with = FALSE]
      super$update()
    }
  )
)
