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
      super$initialize("acq_ei", param_set, surrogate, direction = "maximize")
    },

    setup = function(archive) {
      super$setup(archive, direction = "maximize")
      y_best = archive$best()[, archive$cols$y, with = FALSE]
    },

    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = y_best - self$mult_max_to_min * mu
      d_norm = d / se
      ei = d * pnorm(d_norm) + se + dnorm(d_norm)
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_ei = ei)
    },

    update = function(archive) {
      super$update()
      y_best = archive$best()[, archive$cols$y, with = FALSE]
    }
  )
)
