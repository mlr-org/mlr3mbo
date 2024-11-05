#' @title Acquisition Function Stochastic Confidence Bound
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_stochastic_cb
#'
#' @templateVar id stochastic_cb
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Lower / Upper Confidence Bound with lambda sampling and decay.
#' The initial lambda value is drawn from an uniform distribution between `min_lambda` and `max_lambda` or from an exponential distribution with rate `1 / lambda`.
#' The lambda value is updated after each evaluation by the formula `lambda * exp(-rate * (t %% period))`.
#'
#' @section Parameters:
#' * `"lambda"` (`numeric(1)`)\cr
#'   Lambda value for sampling from the exponential distribution.
#'   Defaults to `1.96`.
#' * `"min_lambda"` (`numeric(1)`)\cr
#'   Minimum value of lambda for sampling from the uniform distribution.
#'   Defaults to `0.01`.
#' * `"max_lambda"` (`numeric(1)`)\cr
#'   Maximum value of lambda for sampling from the uniform distribution.
#'   Defaults to `10`.
#' * `"distribution"` (`character(1)`)\cr
#'   Distribution to sample lambda from.
#'   One of `c("uniform", "exponential")`.
#'   Defaults to `uniform`.
#' * `"rate"` (`numeric(1)`)\cr
#'   Rate of the exponential decay.
#'   Defaults to `0` i.e. no decay.
#' * `"period"` (`integer(1)`)\cr
#'   Period of the exponential decay.
#'   Defaults to `NULL` i.e. the decay has no period.
#'
#' @references
#' * `r format_bib("snoek_2012")`
#' * `r format_bib("egele_2023")`
#'
#' @family Acquisition Function
#' @export
AcqFunctionStochasticCB = R6Class("AcqFunctionStochasticCB",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    #' @param lambda (`numeric(1)`).
    #' @param min_lambda (`numeric(1)`).
    #' @param max_lambda (`numeric(1)`).
    #' @param distribution (`character(1)`).
    #' @param rate (`numeric(1)`).
    #' @param period (`integer(1)`).
    initialize = function(
      surrogate = NULL,
      lambda = 1.96,
      min_lambda = 0.01,
      max_lambda = 10,
      distribution = "uniform",
      rate = 0,
      period = NULL
      ) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      private$.lambda = assert_number(lambda, lower = .Machine$double.neg.eps, null.ok = TRUE)
      private$.min_lambda = assert_number(min_lambda, lower = .Machine$double.neg.eps, null.ok = TRUE)
      private$.max_lambda = assert_number(max_lambda, lower = .Machine$double.neg.eps, null.ok = TRUE)
      private$.distribution = assert_choice(distribution, choices = c("uniform", "exponential"))

      if (private$.distribution == "uniform" && (is.null(private$.min_lambda) || is.null(private$.max_lambda))) {
        stop("If `distribution` is 'uniform', `min_lambda` and `max_lambda` must be set.")
      }

      if (private$.distribution == "exponential" && is.null(private$.lambda)) {
        stop("If `distribution` is 'exponential', `lambda` must be set.")
      }

      private$.rate = assert_number(rate, lower = 0)
      private$.period = assert_int(period, lower = 1, null.ok = TRUE)

      constants = ps(
        lambda = p_dbl(lower = 0)
      )

      super$initialize("acq_cb",
        constants = constants,
        surrogate = surrogate,
        requires_predict_type_se = TRUE,
        direction = "same",
        label = "Stochastic Lower / Upper Confidence Bound",
        man = "mlr3mbo::mlr_acqfunctions_stochastic_cb")
    },

    #' @description
    #' Update the acquisition function.
    #' Samples and decays lambda.
    update = function() {
      # sample lambda
      if (is.null(self$constants$values$lambda)) {

        if (private$.distribution == "uniform") {
          lambda = runif(1, private$.min_lambda, private$.max_lambda)
        } else {
          lambda = rexp(1, 1 / private$.lambda)
        }

        private$.lambda_0 = lambda
        self$constants$values$lambda = lambda
      }

      # decay lambda
      if (private$.rate > 0) {
        lambda_0 = private$.lambda_0
        period = private$.period
        t = if (is.null(period)) private$.t else private$.t %% period
        rate = private$.rate

        self$constants$values$lambda = lambda_0 * exp(-rate * t)
        private$.t = t + 1
      }
    }
  ),

  private = list(
    .lambda = NULL,
    .min_lambda = NULL,
    .max_lambda = NULL,
    .distribution = NULL,
    .rate = NULL,
    .period = NULL,
    .t = 0,
    .lambda_0 = NULL,

    .fun = function(xdt, lambda) {
      p = self$surrogate$predict(xdt)
      cb = p$mean - self$surrogate_max_to_min * lambda * p$se
      data.table(acq_cb = cb,  acq_lambda = lambda, acq_lambda_0 = private$.lambda_0)
    }
  )
)

mlr_acqfunctions$add("stochastic_cb", AcqFunctionStochasticCB)

