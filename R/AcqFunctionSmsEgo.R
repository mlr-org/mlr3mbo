#' @title Acquisition Function SMS-EGO
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_smsego
#'
#' @description
#' S-Metric Selection Evolutionary Multi-Objective Optimization Algorithm Acquisition Function.
#'
#' @section Parameters:
#' * `"lambda"` (`numeric(1)`)\cr
#'   \eqn{\lambda} value used for the confidence bound.
#'   Defaults to `1`.
#'   Based on \code{confidence = (1 - 2 * dnorm(lambda)) ^ m} you can calculate a
#'   lambda for a given confidence level, see Ponweiser et al. (2008).
#' * `"epsilon"` (`numeric(1)`)\cr
#'   \eqn{\epsilon} used for the additive epsilon dominance.
#'   Can either be a single numeric value > 0 or `NULL` (default).
#'   In the case of being `NULL`, an epsilon vector is maintained dynamically as
#'   described in Horn et al. (2015).
#'
#' @references
#' * `r format_bib("ponweiser_2008")`
#' * `r format_bib("horn_2015")`
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
#'     list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceMultiCrit$new(
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
#'
#'   acq_function = acqf("smsego", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$progress = 5 - 4 # n_evals = 5 and 4 points already evaluated
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionSmsEgo = R6Class("AcqFunctionSmsEgo",
  inherit = AcqFunction,

  public = list(

    #' @field ys_front (`matrix()`)\cr
    #'   Approximated Pareto front.
    ys_front = NULL,

    #' @field ref_point (`numeric()`)\cr
    #'   Reference point.
    ref_point = NULL,

    #' @field epsilon (`numeric()`)\cr
    #'  Epsilon used for the additive epsilon dominance.
    epsilon = NULL,

    #' @field progress (`numeric(1)`)\cr
    #'   Optimization progress (typically, the number of function evaluations left).
    #'   Note that this requires the [bbotk::OptimInstance] to be terminated via a [bbotk::TerminatorEvals].
    progress = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearnerCollection]).
    #' @param lambda (`numeric(1)`).
    #' @param epsilon (`NULL` | `numeric(1)`).
    initialize = function(surrogate = NULL, lambda = 1, epsilon = NULL) {
      assert_r6(surrogate, "SurrogateLearnerCollection", null.ok = TRUE)
      assert_number(lambda, lower = 1, finite = TRUE)
      assert_number(epsilon, lower = 0, finite = TRUE, null.ok = TRUE)

      constants = ParamSet$new(list(
        ParamDbl$new("lambda", lower = 0, default = 1),
        ParamDbl$new("epsilon", lower = 0, default = NULL, special_vals = list(NULL))  # for NULL, it will be calculated dynamically
      ))
      constants$values$lambda = lambda
      constants$values$epsilon = epsilon

      super$initialize("acq_smsego", constants = constants, surrogate = surrogate, direction = "minimize", label = "SMS-EGO", man = "mlr3mbo::mlr_acqfunctions_smsego")  # indeed, we minimize, see comments below about C code
    },

    #' @description
    #' Updates acquisition function and sets `ys_front`, `ref_point`, `epsilon`.
    update = function() {
      if (is.null(self$progress)) {
        stop("$progress is not set.")  # needs self$progress here! Originally self$instance$terminator$param_set$values$n_evals - archive$n_evals
      }

      n_obj = length(self$archive$cols_y)
      ys = self$archive$data[, self$archive$cols_y, with = FALSE]
      ys = as.matrix(ys) %*% diag(self$surrogate_max_to_min)
      self$ys_front = as.matrix(self$archive$best()[, self$archive$cols_y, with = FALSE]) %*% diag(self$surrogate_max_to_min)
      self$ref_point = apply(ys, MARGIN = 2L, FUN = max) + 1  # offset = 1 like in mlrMBO

      if (is.null(self$constants$values$epsilon)) {
        # The following formula is taken from Horn et al. 2015.
        # Note that the one in Ponweiser et al. 2008 has a typo.
        # Note that nrow(self$ys_front) is correct and mlrMBO has a bug https://github.com/mlr-org/mlrMBO/blob/2dd83601ed80030713dfe0f55d4a5b8661919ce1/R/infill_crits.R#L292
        c_val = 1 - (1 / (2 ^ n_obj))
        epsilon = map_dbl(
          seq_col(self$ys_front),
          function(i) {
            (max(self$ys_front[, i]) - min(self$ys_front[, i])) / (nrow(self$ys_front) + c_val * self$progress)
          }
        )
        self$epsilon = epsilon
      } else {
        self$epsilon = self$constants$values$epsilon
      }
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      constants = list(...)
      lambda = constants$lambda
      if (is.null(self$ys_front)) {
        stop("$ys_front is not set. Missed to call $update()?")
      }
      if (is.null(self$epsilon)) {
        stop("$epsilon is not set. Missed to call $update()?")
      }
      if (is.null(self$ref_point)) {
        stop("$ref_point is not set. Missed to call $update()?")
      }
      ps = self$surrogate$predict(xdt)
      means = map_dtc(ps, "mean")
      ses = map_dtc(ps, "se")
      cbs = as.matrix(means) %*% diag(self$surrogate_max_to_min) - lambda * as.matrix(ses)
      # allocate memory for adding points to front for HV calculation in C
      front2 = t(rbind(self$ys_front, 0))
      sms = .Call("c_sms_indicator", PACKAGE = "mlr3mbo", cbs, self$ys_front, front2, self$epsilon, self$ref_point)  # note that the negative indicator is returned from C
      data.table(acq_smsego = sms)
    }
  )
)

mlr_acqfunctions$add("smsego", AcqFunctionSmsEgo)

