#' @title Acquisition Function Mean
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_mean
#'
#' @templateVar id mean
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Mean.
#'
#' @family Acquisition Function
#' @export
AcqFunctionMean = R6Class("AcqFunctionMean",
  inherit = AcqFunction,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      super$initialize("acq_mean", surrogate = surrogate, direction = "minimize")
    }
  ),

  private = list(
    .fun = function(xdt) {
      p = self$surrogate$predict(xdt)
      mu = self$surrogate_max_to_min * p$mean
      data.table(acq_mean = mu)
    }
  )
)

mlr_acqfunctions$add("mean", AcqFunctionMean)
