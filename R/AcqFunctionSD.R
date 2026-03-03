#' @title Acquisition Function Standard Deviation
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_sd
#'
#' @templateVar id sd
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Posterior Standard Deviation.
#'
#' @family Acquisition Function
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'   library(data.table)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = default_gp()
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("sd", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionSD = R6Class("AcqFunctionSD",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL) {
      super$initialize("acq_sd", surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Posterior Standard Deviation", man = "mlr3mbo::mlr_acqfunctions_sd")
    },

    #' @description
    #' Validate that the surrogate is a [SurrogateLearner] compatible with this acquisition function.
    #'
    #' @param surrogate ([SurrogateLearner])\cr
    #'   Surrogate to validate.
    #'
    #' @return The validated [SurrogateLearner].
    check_surrogate = function(surrogate) {
      assert_r6(surrogate, classes = "SurrogateLearner")
      if (self$requires_predict_type_se && surrogate$predict_type != "se") {
        error_config("Acquisition function '%s' requires the surrogate to have 'se' as predict_type.", class(self)[[1L]])
      }
      surrogate
    }
  ),

  private = list(
    .fun = function(xdt) {
      p = self$surrogate$predict(xdt)
      data.table(acq_sd = p$se)
    }
  )
)

mlr_acqfunctions$add("sd", AcqFunctionSD)

