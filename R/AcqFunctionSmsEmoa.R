#' @title Acquisition function: SMS EMOA
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' @section Fields: See [AcqFunction]
#' @section Methods: See [AcqFunction]
#' @export
AcqFunctionSmsEmoa = R6Class("AcqFunctionSmsEmoa",
  inherit = AcqFunction, #FIXME: AcqFunctionMultiCrit?
  public = list(

    initialize = function(surrogate) { # FIXME: If we have a multi-output learner we might only want to use this learner as a single surrogate, alternatively we might want to have a SurrogateMulti class that abstracts this idea and allows both (a MultiOutput learner or multiple single learners)
      param_set = ParamSet$new(list(
        ParamDbl$new("lambda", lower = 0, default = 1)
        ParamDbl$new("eps", lower = 0, default = NULL) # for NULL, it will be calculated dynamically
      ))
      param_set$values$lambda = 1
      assert_list(surrogate, types = "SurrogateMultiCrit")
      super$initialize("acq_sms", param_set, surrogate = surrogate, direction = "maximize")
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
