#' @title Acquisition Function Expected Improvement Per Second
#'
#' @description
#' Expected Improvement per second.
#'
#' TODO DESCRIPTION and Reference
#'
#' @references
#' `r format_bib("snoek_2012")`
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionEIPS = R6Class("AcqFunctionEIPS",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateSingleCrit]).
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateMultiCrit")

      if (surrogate$k != 2) {
        stop("SurrogateMultiCrit for k=2 needed")
      }

      fun = function(xdt) {
        if (is.null(self$y_best)) {
          stop("y_best is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        mu = p[[1]]$mean
        se = p[[1]]$se
        mu_t = p[[2]]$mean
        d = self$y_best - self$surrogate_max_to_min * mu
        d_norm = d / se
        ei = d * pnorm(d_norm) + se + dnorm(d_norm)
        eips = ei / mu_t
        eips = ifelse(se < 1e-20 | mu_t < 1e-20, 0, ei)
        data.table(acq_eips = eips)
      }

      super$initialize("acq_eips", surrogate = surrogate, direction = "maximize", fun = fun)
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    #'
    #' @param archive ([bbotk::Archive]).
    update = function(archive) {
      super$update(archive)
      y = archive$data[,archive$cols_y, with = FALSE][[1]]
      y = y * mult_max_to_min(archive$codomain)
      self$y_best = min(y)
    }
  )
)
