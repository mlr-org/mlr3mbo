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
      assert_r6(surrogate, "SurrogateSingleCrit")

      fun = function(xdt) {
        if (is.null(self$y_best)) {
          stop("y_best is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        mu = p$mean
        se = p$se
        d = self$y_best - self$surrogate_max_to_min * mu
        d_norm = d / se
        ei = d * pnorm(d_norm) + se * dnorm(d_norm)
        ei = ifelse(se < 1e-20, 0, ei)
        data.table(acq_ei = ei)
      }

      super$initialize("acq_ei", surrogate = surrogate, direction = "maximize", fun = fun)
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
