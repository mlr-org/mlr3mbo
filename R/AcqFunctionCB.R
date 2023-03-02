#' @title Acquisition Function Confidence Bound
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_cb
#'
#' @templateVar id cb
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Lower / Upper Confidence Bound.
#'
#' @section Parameters:
#' * `"lambda"` (`numeric(1)`)\cr
#'   \eqn{\lambda} value used for the confidence bound.
#'   Defaults to `2`.
#'
#' @references
#' * `r format_bib("snoek_2012")`
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
#'   instance = OptimInstanceSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = lrn("regr.km",
#'     covtype = "matern3_2",
#'     optim.method = "gen",
#'     nugget.stability = 10^-8,
#'     control = list(trace = FALSE))
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("cb", surrogate = surrogate, lambda = 3)
#'
#'   acq_function$surrogate$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionCB = R6Class("AcqFunctionCB",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    #' @param lambda (`numeric(1)`).
    initialize = function(surrogate = NULL, lambda = 2) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      assert_number(lambda, lower = 0, finite = TRUE)

      constants = ps(lambda = p_dbl(lower = 0, default = 2))
      constants$values$lambda = lambda

      super$initialize("acq_cb", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "same", label = "Lower / Upper Confidence Bound", man = "mlr3mbo::mlr_acqfunctions_cb")
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      constants = list(...)
      lambda  = constants$lambda
      p = self$surrogate$predict(xdt)
      res = p$mean - self$surrogate_max_to_min * lambda * p$se
      data.table(acq_cb = res)
    }
  )
)

mlr_acqfunctions$add("cb", AcqFunctionCB)

