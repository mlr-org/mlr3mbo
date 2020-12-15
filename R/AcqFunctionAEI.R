#' @title Acquisition Function Augmented Expected Improvement
#'
#' @description
#' Augmented Expected Improvement.
#'
#' @references
#' `r format_bib("huang_2012")`
#'
#' @section Parameters:
#' * `c` (`numeric(1)`)\cr
#'   Constant \eqn{c} as used in formula (14) of Huang 2012 to reflect the
#'   degree of risk aversion. Defaults to `1`.
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionAEI = R6Class("AcqFunctionAEI",
  inherit = AcqFunction,
  public = list(

    #' @field y_effective_best (`numeric()`).
    y_effective_best = NULL,

    #' @field noise_var (`numeric()`).
    noise_var = NULL, # noise of the function

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      param_set = ParamSet$new(list(
        ParamDbl$new("c", lower = 0, default = 1)
      ))
      param_set$values$c = 1
      assert_r6(surrogate, "SurrogateSingleCrit")
      super$initialize("acq_aei", param_set, surrogate, direction = "maximize")
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table()]
    #'
    #' @return [data.table::data.table()].
    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = self$y_effective_best - self$surrogate_max_to_min * mu
      d_norm = d / se
      aei = d * pnorm(d_norm) + se * dnorm(d_norm) * (1 - sqrt(self$noise_var) / sqrt(self$noise_var + se^2))
      aei = ifelse(se < 1e-20, 0, aei)
      data.table(acq_aei = aei)
    },

    #' @description
    #' Updates acquisition function and sets `y_effective_best`.
    #'
    #' @param archive [bbotk::Archive]
    update = function(archive) {
      super$update(archive)
      xdt = archive$data()[, archive$cols_x, with = FALSE]
      p = self$surrogate$predict(xdt)
      if (self$surrogate_max_to_min == 1) { # minimization
        y_effective = p$mean + self$param_set$values$c * p$se # pessimistic prediction
        self$y_effective_best = min(y_effective)
      } else { # maximization
        y_effective = p$mean - self$param_set$values$c * p$se # pessimistic prediction
        self$y_effective_best = max(y_effective)
      }

      if (!is.null(self$surrogate$model$model) && length(self$surrogate$model$model@covariance@nugget) == 1) {
        self$noise_var = self$surrogate$model$model@covariance@nugget #FIXME: Check that this value really exists (otherwise calculate residual variance?)
      } else {
        lgr$warn("AEI currently only works correctly with regr.km and nugget estim = TRUE or given!")
        self$noise_var = 0
      }


    }
  )
)
