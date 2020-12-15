#' @title Acquisition Function Expected Improvement
#'
#' @description
#' Expected Improvement.
#'
#' TODO DESCRIPTION and Reference
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionEI = R6Class("AcqFunctionEI",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      param_set = ParamSet$new()
      assert_r6(surrogate, "SurrogateSingleCrit")
      super$initialize("acq_ei", param_set, surrogate, direction = "maximize")
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table()]
    #'
    #' @return [data.table::data.table()].
    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = self$y_best - self$surrogate_max_to_min * mu
      d_norm = d / se
      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_ei = ei)
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    #'
    #' @param archive [bbotk::Archive]
    update = function(archive) {
      super$update(archive)
      self$y_best = archive$best()[[archive$cols_y]]
    }
  )
)
