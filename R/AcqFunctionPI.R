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
#' * `r format_bib("kushner_1964")`
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
#'   acq_function = acqf("pi", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionPI = R6Class("AcqFunctionPI",
  inherit = AcqFunction,

  public = list(

    #' @field y_best (`numeric(1)`)\cr
    #'   Best objective function value observed so far.
    #'   In the case of maximization, this already includes the necessary change of sign.
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      super$initialize("acq_pi", surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Probability Of Improvement", man = "mlr3mbo::mlr_acqfunctions_pi")
    },

    #' @description
    #' Update the acquisition function and set `y_best`.
    update = function() {
      y = self$archive$data[, self$surrogate$cols_y, with = FALSE]
      if (self$surrogate$output_trafo_must_be_considered) {
        y = self$surrogate$output_trafo$transform(y)
      }
      self$y_best = min(self$surrogate_max_to_min * y)
    }
  ),

  private = list(
    .fun = function(xdt) {
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
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

