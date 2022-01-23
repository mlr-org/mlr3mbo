#' @title Acquisition Function Probability of Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_pi
#'
#' @templateVar id pi
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Probability of Improvement.
#'
#' @references
#' `r format_bib("kushner_1964")`
#'
#' @family Acquisition Function
#' @export
AcqFunctionPI = R6Class("AcqFunctionPI",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      super$initialize("acq_pi", surrogate = surrogate, direction = "maximize")
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    update = function() {
      self$y_best = min(self$surrogate_max_to_min * self$archive$data[[self$surrogate$y_cols]])
    }
  ),

  private = list(
    .fun = function(xdt) {
      if (is.null(self$y_best)) {
        stop("y_best is not set. Missed to call $update()?")
      }
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d_norm = (self$y_best - self$surrogate_max_to_min * mu) / se
      probi = pnorm(d_norm)
      probi = ifelse(se < 1e-20, 0, probi)
      data.table(acq_pi = probi)
    }
  )
)

mlr_acqfunctions$add("pi", AcqFunctionPI)
