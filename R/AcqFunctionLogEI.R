#' @title Acquisition Function Log Expected Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_log_ei
#'
#' @templateVar id log_ei
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Improvement assuming that the target variable is to be minimized and has been modeled on log scale.
#'
#' @family Acquisition Function
#' @export
AcqFunctionLogEI = R6Class("AcqFunctionLogEI",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric(1)`)\cr
    #'   Best objective function value observed so far.
    #'   On log scale.
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL, epsilon = 0) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      assert_number(epsilon, lower = 0, finite = TRUE)

      constants = ps(epsilon = p_dbl(lower = 0, default = 0))
      constants$values$epsilon = epsilon

      super$initialize("acq_log_ei", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Log Expected Improvement", man = "mlr3mbo::mlr_acqfunctions_log_ei")
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    update = function() {
      if (self$surrogate_max_to_min != 1L) {
        stop("Log EI assumes minimization of the log transformed target value.")
      }
      self$y_best = min(self$surrogate_max_to_min * self$archive$data[[self$surrogate$cols_y]])
    }
  ),
  private = list(
    .fun = function(xdt, ...) {
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
      }
      if (self$surrogate_max_to_min != 1L) {
        stop("Log EI assumes minimization of the log transformed target value.")
      }
      constants = list(...)
      epsilon = constants$epsilon
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = self$y_best - mu - epsilon
      d_norm = d / se
      log_ei = (exp(self$y_best) * pnorm(d_norm)) - (exp((0.5 * se^2) + mu) * pnorm(d_norm - se))
      log_ei = ifelse(se < 1e-20, 0, log_ei)
      data.table(acq_log_ei = log_ei)
    }
  )
)

mlr_acqfunctions$add("log_ei", AcqFunctionLogEI)
