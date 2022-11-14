#' @title Acquisition Function Augmented Expected Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_aei
#'
#' @templateVar id aei
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Augmented Expected Improvement.
#' Currently only works correctly with `"regr.km"` as surrogate model and `nugget.estim = TRUE` or given.
#'
#' @section Parameters:
#' * `"c"` (`numeric(1)`)\cr
#'   Constant \eqn{c} as used in formula (14) of Huang (2012) to reflect the degree of risk aversion. Defaults to `1`.
#'
#' @references
#' * `r format_bib("huang_2012")`
#'
#' @family Acquisition Function
#' @export
AcqFunctionAEI = R6Class("AcqFunctionAEI",
  inherit = AcqFunction,

  public = list(

    #' @field y_effective_best (`numeric(1)`).
    y_effective_best = NULL,

    #' @field noise_var (`numeric(1)`).
    noise_var = NULL,  # noise of the function

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    #' @param c (`numeric(1)`).
    initialize = function(surrogate = NULL, c = 1) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      assert_number(c, lower = 0, finite = TRUE)

      constants = ps(c = p_dbl(lower = 0, default = 1))
      constants$values$c = c

      super$initialize("acq_aei", constants = constants, surrogate = surrogate, direction = "maximize", label = "Augmented Expected Improvement", man = "mlr3mbo::mlr_acqfunctions_aei")
    },

    #' @description
    #' Updates acquisition function and sets `y_effective_best`.
    update = function() {
      xdt = self$archive$data[, self$archive$cols_x, with = FALSE]
      p = self$surrogate$predict(xdt)
      if (self$surrogate_max_to_min == 1) {  # minimization
        y_effective = p$mean + self$constants$values$c * p$se  # pessimistic prediction
        self$y_effective_best = min(y_effective)
      } else {  # maximization
        y_effective = p$mean - self$constants$values$c * p$se  # pessimistic prediction
        self$y_effective_best = max(y_effective)
      }

      if (!is.null(self$surrogate$model$model) && length(self$surrogate$model$model@covariance@nugget) == 1L) {
        self$noise_var = self$surrogate$model$model@covariance@nugget  # FIXME: check that this value really exists (otherwise calculate residual variance?)
      } else {
        lgr$warn('AEI currently only works correctly with `"regr.km"` as surrogate model and `nugget.estim = TRUE` or given!')
        self$noise_var = 0
      }

    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      if (is.null(self$y_effective_best)) {
        stop("$y_effective_best is not set. Missed to call $update()?")
      }
      if (is.null(self$noise_var)) {
        stop("$noise_var is not set. Missed to call $update()?")
      }
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = self$y_effective_best - self$surrogate_max_to_min * mu
      d_norm = d / se
      aei = d * pnorm(d_norm) + se * dnorm(d_norm) * (1 - sqrt(self$noise_var) / sqrt(self$noise_var + se^2))
      aei = ifelse(se < 1e-20, 0, aei)
      data.table(acq_aei = aei)
    }
  )
)

mlr_acqfunctions$add("aei", AcqFunctionAEI)
