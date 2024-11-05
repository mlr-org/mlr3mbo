#' @title Acquisition Function Stochastic Expected Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_stochastic_ei
#'
#' @templateVar id stochastic_ei
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Improvement with epsilon decay.
#'
#' @section Parameters:
#' * `"epsilon"` (`numeric(1)`)\cr
#'   \eqn{\epsilon} value used to determine the amount of exploration.
#'   Higher values result in the importance of improvements predicted by the posterior mean
#'   decreasing relative to the importance of potential improvements in regions of high predictive uncertainty.
#'   Defaults to `0.1`.
#' * `"rate"` (`numeric(1)`)\cr
#    Rate of the exponential decay.
#'   Defaults to `0.05`.
#' * `"period"` (`integer(1)`)\cr
#'   Period of the exponential decay.
#'   Defaults to `NULL` i.e. the decay has no period.
#'
#' @references
#' * `r format_bib("jones_1998")`
#'
#' @family Acquisition Function
#' @export
AcqFunctionStochasticEI = R6Class("AcqFunctionStochasticEI",
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
    #' @param rate (`numeric(1)`).
    #' @param period (`integer(1)`).
    initialize = function(
      surrogate = NULL,
      epsilon = 0.1,
      rate = 0.05,
      period = NULL
      ) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      private$.epsilon_0 = assert_number(epsilon, lower = 0, finite = TRUE)
      private$.rate = assert_number(rate, lower = 0, finite = TRUE)
      private$.period = assert_int(period, lower = 1, null.ok = TRUE)

      constants = ps(
        epsilon = p_dbl(lower = 0)
      )

      super$initialize("acq_ei",
        constants = constants,
        surrogate = surrogate,
        requires_predict_type_se = TRUE,
        direction = "maximize",
        label = "Stochastic Expected Improvement",
        man = "mlr3mbo::mlr_acqfunctions_stochastic_ei")
    },

    #' @description
    #' Update the acquisition function.
    #' Sets `y_best` to the best observed objective function value.
    #' Decays epsilon.
    update = function() {
      self$y_best = min(self$surrogate_max_to_min * self$archive$data[[self$surrogate$cols_y]])

      # decay epsilon
      epsilon_0 = private$.epsilon_0
      period = private$.period
      t = if (is.null(period)) private$.t else private$.t %% period
      rate = private$.rate

      self$constants$values$epsilon = epsilon_0 * exp(-rate * t)
      private$.t = t + 1
    }
  ),

  private = list(
    .rate = NULL,
    .period = NULL,
    .epsilon_0 = NULL,
    .t = 0,

    .fun = function(xdt, epsilon) {
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
      }
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = (self$y_best - self$surrogate_max_to_min * mu) - epsilon
      d_norm = d / se
      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_ei = ei, acq_epsilon = epsilon, acq_epsilon_0 = private$.epsilon_0)
    }
  )
)

mlr_acqfunctions$add("stochastic_ei", AcqFunctionStochasticEI)

