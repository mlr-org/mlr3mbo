#' @title Acquisition Function Expected Improvement Per Second
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_eips
#'
#' @templateVar id eips
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Improvement per Second.
#'
#' It is assumed that calculations are performed on an [bbotk::OptimInstanceBatchSingleCrit].
#' Additionally to target values of the codomain that should be minimized or maximized, the
#' [bbotk::Objective] of the [bbotk::OptimInstanceBatchSingleCrit] should return time values.
#' The column names of the target variable and time variable must be passed as `cols_y` in the
#' order `(target, time)` when constructing the [SurrogateLearnerCollection] that is being used as a
#' surrogate.
#'
#' @references
#' * `r format_bib("snoek_2012")`
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
#'     list(y = xs$x ^ 2, time = abs(xs$x))
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time"))
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
#'   surrogate = srlrn(list(learner, learner$clone(deep = TRUE)), archive = instance$archive)
#'   surrogate$cols_y = c("y", "time")
#'
#'   acq_function = acqf("eips", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionEIPS = R6Class("AcqFunctionEIPS",
  inherit = AcqFunction,

  public = list(

    #' @field y_best (`numeric(1)`)\cr
    #'   Best objective function value observed so far.
    #'   In the case of maximization, this already includes the necessary change of sign.
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearnerCollection]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearnerCollection", null.ok = TRUE)
      # FIXME: check that col_y, col_time is the same as surrogate$cols_y?

      super$initialize("acq_eips", surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "Expected Improvement Per Second", man = "mlr3mbo::mlr_acqfunctions_eips")
    },

    #' @description
    #' Update the acquisition function and set `y_best`.
    update = function() {
      ys = self$archive$data[, self$surrogate$cols_y, with = FALSE]
      if (self$surrogate$output_trafo_must_be_considered) {
        ys = self$surrogate$output_trafo$transform(ys)
      }
      self$y_best = min(self$surrogate_max_to_min[[self$col_y]] * ys[[self$col_y]])
    }
  ),

  active = list(

    #' @field col_y (`character(1)`).
    col_y = function(rhs) {
      if (!missing(rhs)) {
        stop("$col_y is read-only.")
      }
      self$archive$cols_y
    },

    #' @field col_time (`character(1)`).
    col_time = function(rhs) {
      if (!missing(rhs)) {
        stop("$col_time is read-only.")
      }
      col_time = self$archive$codomain$ids(tags = "time")
      if (length(col_time) != 1L) {
        stop("Need exactly one parameter in the codomain tagged as 'time'.")
      }
      col_time
    }
  ),

  private = list(
    .fun = function(xdt) {
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
      }
      p = self$surrogate$predict(xdt)
      mu = p[[self$col_y]]$mean
      se = p[[self$col_y]]$se
      mu_t = p[[self$col_time]]$mean
      d = self$y_best - self$surrogate_max_to_min[[self$col_y]] * mu
      d_norm = d / se
      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
      eips = ei / mu_t
      eips = ifelse(se < 1e-20 | mu_t < 1e-20, 0, ei)
      data.table(acq_eips = eips)
    }
  )
)

mlr_acqfunctions$add("eips", AcqFunctionEIPS)

