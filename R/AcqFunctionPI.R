#' @title Acquisition Function Probability of Improvement
#'
#' @description
#' Probability of Improvement.
#'
#' See Kushner (1964).
#'
#' @references
#' `r format_bib("ding_2010")`
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionPI = R6Class("AcqFunctionPI",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateSingleCrit]).
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateSingleCrit")

      fun = function(xdt) {
        if (is.null(self$y_best)) {
          stop("y_best is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        mu = p$mean
        se = p$se
        d_norm = (self$y_best - self$surrogate_max_to_min * mu) / se
        probi = pnorm(d_norm)
        probi = ifelse(se < 1e-20, 0, probi)
        data.table(acq_pi = probi)
      }

      super$initialize("acq_pi", surrogate = surrogate, direction = "maximize", fun = fun)
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    #'
    #' @param archive ([bbotk::Archive]).
    update = function(archive) {
      super$update(archive)
      self$y_best = archive$best()[[archive$cols_y]]
    }
  )
)
