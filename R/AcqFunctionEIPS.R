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
#' It is assumed that calculations are performed on an [bbotk::OptimInstanceSingleCrit].
#' Additionally to target values of the codomain that should be minimized or maximized, the
#' [bbotk::Objective] of the [bbotk::OptimInstanceSingleCrit] should return time values.
#' The column names of the target variable and time variable must be passed as `y_cols` in the
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
#'   surrogate = srlrnc(list(learner, learner$clone(deep = TRUE)), archive = instance$archive)
#'   surrogate$y_cols = c("y", "time")
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
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearnerCollection]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearnerCollection", null.ok = TRUE)
      # FIXME: check that y_col, time_col is the same as surrogate$y_cols?

      super$initialize("acq_eips", surrogate = surrogate, direction = "maximize", label = "Expected Improvement Per Second", man = "mlr3mbo::mlr_acqfunctions_eips")
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    update = function() {
      self$y_best = min(self$surrogate_max_to_min[[self$y_col]] * self$archive$data[[self$y_col]])
    }
  ),

  active = list(

    #' @field y_col (`character(1)`).
    y_col = function(rhs) {
      if (!missing(rhs)) {
        stop("$y_col is read-only.")
      }
      self$archive$cols_y
    },

    #' @field time_col (`character(1)`).
    time_col = function(rhs) {
      if (!missing(rhs)) {
        stop("$time_col is read-only.")
      }
      time_col = self$archive$codomain$ids(tags = "time")
      if (length(time_col) != 1L) {
        stop("Need exactly one parameter in the codomain tagged as 'time'.")
      }
      time_col
    }
  ),

  private = list(
    .fun = function(xdt) {
      if (is.null(self$y_best)) {
        stop("$y_best is not set. Missed to call $update()?")
      }
      p = self$surrogate$predict(xdt)
      mu = p[[self$y_col]]$mean
      se = p[[self$y_col]]$se
      mu_t = p[[self$time_col]]$mean
      d = self$y_best - self$surrogate_max_to_min[[self$y_col]] * mu
      d_norm = d / se
      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
      eips = ei / mu_t
      eips = ifelse(se < 1e-20 | mu_t < 1e-20, 0, ei)
      data.table(acq_eips = eips)
    }
  )
)

mlr_acqfunctions$add("eips", AcqFunctionEIPS)

