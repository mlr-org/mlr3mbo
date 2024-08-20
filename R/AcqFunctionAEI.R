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
#' Useful when working with noisy objectives.
#' Currently only works correctly with `"regr.km"` as surrogate model and `nugget.estim = TRUE` or given.
#'
#' @section Parameters:
#' * `"c"` (`numeric(1)`)\cr
#'   Constant \eqn{c} as used in Formula (14) of Huang (2012) to reflect the degree of risk aversion. Defaults to `1`.
#'
#' @references
#' * `r format_bib("huang_2012")`
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
#'   set.seed(2906)
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2 + rnorm(length(xs$x), mean = 0, sd = 1))
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun,
#'     domain = domain,
#'     codomain = codomain,
#'     properties = "noisy")
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = lrn("regr.km",
#'     covtype = "matern5_2",
#'     optim.method = "gen",
#'     nugget.estim = TRUE,
#'     jitter = 1e-12,
#'     control = list(trace = FALSE))
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("aei", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionAEI = R6Class("AcqFunctionAEI",
  inherit = AcqFunction,

  public = list(

    #' @field y_effective_best (`numeric(1)`)\cr
    #'   Best effective objective value observed so far.
    #'   In the case of maximization, this already includes the necessary change of sign.
    y_effective_best = NULL,

    #' @field noise_var (`numeric(1)`)\cr
    #'   Estimate of the variance of the noise.
    #'   This corresponds to the `nugget` estimate when using a [mlr3learners][mlr3learners::mlr_learners_regr.km] as surrogate model.
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

      super$initialize("acq_aei", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Augmented Expected Improvement", man = "mlr3mbo::mlr_acqfunctions_aei")
    },

    #' @description
    #' Update the acquisition function and set `y_effective_best` and `noise_var`.
    update = function() {
      xdt = self$archive$data[, self$archive$cols_x, with = FALSE]
      p = self$surrogate$predict(xdt)
      y_effective = p$mean + (self$surrogate_max_to_min * self$constants$values$c * p$se) # pessimistic prediction
      self$y_effective_best = min(self$surrogate_max_to_min * y_effective)

      if (!is.null(self$surrogate$learner$model) && length(self$surrogate$learner$model@covariance@nugget) == 1L) {
        self$noise_var = self$surrogate$learner$model@covariance@nugget  # FIXME: check that this value really exists (otherwise calculate residual variance?)
      } else {
        lgr$warn('AcqFunctionAEI currently only works correctly with `"regr.km"` as surrogate model and `nugget.estim = TRUE` or given.')
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
      aei = (d * pnorm(d_norm) + se * dnorm(d_norm)) * (1 - (sqrt(self$noise_var) / sqrt(se^2L + self$noise_var)))
      aei = ifelse(se < 1e-20, 0, aei)
      data.table(acq_aei = aei)
    }
  )
)

mlr_acqfunctions$add("aei", AcqFunctionAEI)

