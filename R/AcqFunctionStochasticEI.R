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
#' \eqn{\epsilon} is updated after each update by the formula `epsilon * exp(-rate * (t %% period))` where `t` is the number of times the acquisition function has been updated.
#'
#' While this acquisition function usually would be used within an asynchronous optimizer, e.g., [OptimizerAsyncMbo],
#' it can in principle also be used in synchronous optimizers, e.g., [OptimizerMbo].
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
#'   Defaults to `NULL`, i.e., the decay has no period.
#'
#' @section Note:
#' * This acquisition function always also returns its current (`acq_epsilon`) and original (`acq_epsilon_0`) \eqn{\epsilon}.
#'   These values will be logged into the [bbotk::ArchiveBatch] of the [bbotk::OptimInstanceBatch] of the [AcqOptimizer] and
#'   therefore also in the [bbotk::Archive] of the actual [bbotk::OptimInstance] that is to be optimized.
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
#'   acq_function = acqf("stochastic_ei", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
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
    #' @param period (`NULL` | `integer(1)`).
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

      constants = ps(epsilon = p_dbl(lower = 0, default = 0.1))

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
      y = self$archive$data[, self$surrogate$cols_y, with = FALSE]
      if (self$surrogate$output_trafo_must_be_considered) {
        y = self$surrogate$output_trafo$transform(y)
      }
      self$y_best = min(self$surrogate_max_to_min * y)

      # decay epsilon
      epsilon_0 = private$.epsilon_0
      period = private$.period
      t = if (is.null(period)) private$.t else private$.t %% period
      rate = private$.rate

      self$constants$values$epsilon = epsilon_0 * exp(-rate * t)
      private$.t = t + 1L
    },

    #' @description
    #' Reset the acquisition function.
    #' Resets the private update counter `.t` used within the epsilon decay.
    reset = function() {
      private$.t = 0L
    }
  ),

  private = list(
    .rate = NULL,
    .period = NULL,
    .epsilon_0 = NULL,
    .t = 0L,
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

