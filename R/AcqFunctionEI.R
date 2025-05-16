#' @title Acquisition Function Expected Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ei
#'
#' @templateVar id ei
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Improvement.
#'
#' @section Parameters:
#' * `"epsilon"` (`numeric(1)`)\cr
#'   \eqn{\epsilon} value used to determine the amount of exploration.
#'   Higher values result in the importance of improvements predicted by the posterior mean
#'   decreasing relative to the importance of potential improvements in regions of high predictive uncertainty.
#'   Defaults to `0` (standard Expected Improvement).
#'
#' @references
#' * `r format_bib("jones_1998")`
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
#'   acq_function = acqf("ei", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionEI = R6Class("AcqFunctionEI",
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
    #' @param epsilon (`numeric(1)`).
    initialize = function(surrogate = NULL, epsilon = 0) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      assert_number(epsilon, lower = 0, finite = TRUE)

      constants = ps(epsilon = p_dbl(lower = 0, default = 0))
      constants$values$epsilon = epsilon

      super$initialize("acq_ei", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Expected Improvement", man = "mlr3mbo::mlr_acqfunctions_ei")
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
    .fun = function(xdt, ...) {
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
      }
      constants = list(...)
      epsilon = constants$epsilon
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = (self$y_best - self$surrogate_max_to_min * mu) - epsilon
      d_norm = d / se
      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_ei = ei)
    }
  )
)

mlr_acqfunctions$add("ei", AcqFunctionEI)

