#' @title Acquisition Function Top-Two Expected Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ttei
#'
#' @templateVar id ttei
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Top-Two Expected Improvement.
#'
#' With probability `beta` simply the Expected Improvement.
#' With probability `1 - beta` the Expected Improvement over the current Expected Improvement.
#' This requires to first obtain the current argmax of the Expected Improvement to then be able to
#' formulate the Expected Improvement with respect to the posterior prediction of that argmax.
#'
#' @references
#' * `r format_bib("qin_2017")`
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
#'   acq_function = acqf("ttei",
#'     surrogate = surrogate,
#'     toplvl_acq_optimizer = acqo(opt("random_search"), terminator = trm("evals", n_evals = 100)))
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionTTEI = R6Class("AcqFunctionTTEI",
  inherit = AcqFunction,

  public = list(
    #' @field y_best (`numeric(1)`)\cr
    #'   Best objective function value observed so far.
    y_best = NULL,

    #' @field ei_argmax_mean_se ([data.table::data.table()])\cr
    #'   data.table::data.table() containing one row with the surrogate mean and standard error
    #'   prediction for the current argmax of the Expected Improvement.
    ei_argmax_mean_se = NULL,  # FIXME: assert type and format?

    #' @field is_ei (`logical(1)`)\cr
    #'   Whether in the next iteration simply the Expected Improvement is to be optimized or the
    #'   Expected Improvement over the Expected Improvement.
    is_ei = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    #' @param beta (`numeric(1)`)\cr
    #'   With probability `beta` the toplevel Expected Improvement acquisition function is to be
    #'   optimized and with `1 - beta` the Expected Improvement over the Expected Improvement.
    #'   Default is `0.5`.
    #' @param toplvl_acq_optimizer (`NULL` | [AcqOptimizer])\cr
    #'   Acquisition function optimizer for the toplevel Expected Improvement acquisition function
    #'   used to find the current argmax.
    #'   If `NULL` will be initialized to
    #'   `acqo(opt("random_search", batch_size = 1000), terminator = trm("evals",  n_evals = 10000))`.
    initialize = function(surrogate = NULL, beta = 0.5, toplvl_acq_optimizer = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      assert_number(beta, lower = 0, upper = 1)
      assert_r6(toplvl_acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
      if (is.null(toplvl_acq_optimizer)) toplvl_acq_optimizer = acqo(opt("random_search", batch_size = 1000L), terminator = trm("evals",  n_evals = 10000L))
      constants = ps(beta = p_dbl(lower = 0, upper = 1, default = 0.5))
      constants$values$beta = beta
      private$.toplvl_acq_function_ei = AcqFunctionEI$new(surrogate = surrogate)  # FIXME: AB and read-only?
      private$.toplvl_acq_optimizer = toplvl_acq_optimizer$clone(deep = TRUE)  # FIXME: AB and read-only?
      super$initialize("acq_ttei", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Top-Two Expected Improvement", man = "mlr3mbo::mlr_acqfunctions_ttei")
    },

    #' @description
    #' Updates acquisition function and performs the following steps:
    #' * sets `y_best`
    #' * updates the internal toplevel Expected Improvement acquisition function
    #' * samples whether simply the Expected Improvement is to be optimized or the Expected
    #'   Improvement over the Expected Improvement (depending on `beta`) and sets `is_ei` accordingly
    #' * if `is_ei = FALSE`, proceeds to optimize the toplevel Expected Improvement acquisition function to
    #'   find the current argmax and sets `ei_argmax_mean_se`
    update = function() {
      self$y_best = min(self$surrogate_max_to_min * self$archive$data[[self$surrogate$y_cols]])
      private$.toplvl_acq_function_ei$surrogate = self$surrogate
      private$.toplvl_acq_function_ei$update()
      private$.toplvl_acq_optimizer$acq_function = private$.toplvl_acq_function_ei
      if (runif(1, min = 0, max = 1) <= self$constants$values$beta) {
        self$is_ei = TRUE
      } else {
        self$is_ei = FALSE
        ei_argmax = private$.toplvl_acq_optimizer$optimize()
        p_ei_argmax = self$surrogate$predict(ei_argmax)
        self$ei_argmax_mean_se = cbind(ei_argmax, data.table(mean = p_ei_argmax$mean, se = p_ei_argmax$se))
      }
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      constants = list(...)
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
      }
      if (is.null(self$is_ei)) {
        stop("$is_ei is not set. Missed to call $update()?")
      }
      # can xdt_ei_argmax be NULL?
      if (self$is_ei) {
        p = self$surrogate$predict(xdt)
        mu = p$mean
        se = p$se
        d = self$y_best - self$surrogate_max_to_min * mu
        d_norm = d / se
        ei = d * pnorm(d_norm) + se * dnorm(d_norm)
        ei = ifelse(se < 1e-20, 0, ei)
        data.table(acq_ttei = ei, acq_ttei_is_ei = self$is_ei)
      } else {
        p = self$surrogate$predict(xdt)
        mu = p$mean
        se = p$se
        se_overall = sqrt((self$ei_argmax_mean_se$se ^ 2) + (se ^ 2))
        d = (self$surrogate_max_to_min * self$ei_argmax_mean_se$mean - self$surrogate_max_to_min * mu)
        d_norm = d / se_overall
        ttei = se_overall * (d_norm * pnorm(d_norm) + dnorm(d_norm))
        ttei = ifelse(se < 1e-20, 0, ttei)  # FIXME: what about se_overall?
        data.table(acq_ttei = ttei, acq_ttei_is_ei = self$is_ei)
      }
    },

    .toplvl_acq_function_ei = NULL,

    .toplvl_acq_optimizer = NULL
  )
)

mlr_acqfunctions$add("ttei", AcqFunctionTTEI)

