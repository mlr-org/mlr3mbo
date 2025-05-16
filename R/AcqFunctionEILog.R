#' @title Acquisition Function Expected Improvement on Log Scale
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ei_log
#'
#' @templateVar id ei_log
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Improvement assuming that the target variable has been modeled on log scale.
#' In general only sensible if the [SurrogateLearner] uses an [OutputTrafoLog] without inverting the posterior predictive distribution (`invert_posterior = FALSE`).
#' See also the example below.
#'
#' @section Parameters:
#' * `"epsilon"` (`numeric(1)`)\cr
#'   \eqn{\epsilon} value used to determine the amount of exploration.
#'   Higher values result in the importance of improvements predicted by the posterior mean
#'   decreasing relative to the importance of potential improvements in regions of high predictive uncertainty.
#'   Defaults to `0` (standard Expected Improvement).
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
#'   output_trafo = ot("log", invert_posterior = FALSE)
#'
#'   surrogate = srlrn(learner, output_trafo = output_trafo, archive = instance$archive)
#'
#'   acq_function = acqf("ei_log", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionEILog = R6Class("AcqFunctionEILog",
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

      super$initialize("acq_ei_log", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Expected Improvement on Log Scale", man = "mlr3mbo::mlr_acqfunctions_ei_log")
    },

    #' @description
    #' Update the acquisition function and set `y_best`.
    update = function() {
      assert_r6(self$surrogate$output_trafo, "OutputTrafoLog")
      assert_false(self$surrogate$output_trafo$invert_posterior)
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
      assert_r6(self$surrogate$output_trafo, "OutputTrafoLog")
      assert_false(self$surrogate$output_trafo$invert_posterior)
      constants = list(...)
      epsilon = constants$epsilon
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se

      # FIXME: try to unify w.r.t minimization / maximization and the respective transformation
      if (self$surrogate_max_to_min == 1L) {
        # y is to be minimized and the OutputTrafoLog performed the transformation accordingly
        assert_true(self$surrogate$output_trafo$max_to_min == 1L)
        y_best = self$y_best
        d = (y_best - mu) - epsilon
        d_norm = d / se
        multiplicative_factor = (self$surrogate$output_trafo$state[[self$surrogate$output_trafo$cols_y]]$max - self$surrogate$output_trafo$state[[self$surrogate$output_trafo$cols_y]]$min)
        ei_log = multiplicative_factor * ((exp(y_best) * pnorm(d_norm)) - (exp((0.5 * se^2) + mu)) * pnorm(d_norm - se))
      } else {
        # y is to be maximized and the OutputTrafoLog performed the transformation accordingly
        y_best = - self$y_best
        d = (mu - y_best) - epsilon
        d_norm = d / se
        multiplicative_factor = (self$surrogate$output_trafo$state[[self$surrogate$output_trafo$cols_y]]$max - self$surrogate$output_trafo$state[[self$surrogate$output_trafo$cols_y]]$min)
        ei_log = multiplicative_factor * ((exp(-y_best) * pnorm(d_norm)) - (exp((0.5 * se^2) - mu) * pnorm(d_norm - se)))
      }
      ei_log = ifelse(se < 1e-20 | is.na(ei_log), 0, ei_log)
      data.table(acq_ei_log = ei_log)
    }
  )
)

mlr_acqfunctions$add("ei_log", AcqFunctionEILog)

