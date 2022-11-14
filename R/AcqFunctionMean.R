#' @title Acquisition Function Mean
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_mean
#'
#' @templateVar id mean
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Posterior Mean.
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
      super$initialize("acq_mean", surrogate = surrogate, direction = "same", label = "Posterior Mean", man = "mlr3mbo::mlr_acqfunctions_mean")
    }
  ),

  private = list(
    .fun = function(xdt) {
      p = self$surrogate$predict(xdt)
      data.table(acq_mean = p$mean)
    }
  )
)

mlr_acqfunctions$add("mean", AcqFunctionMean)
