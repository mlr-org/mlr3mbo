#' @title Acquisition Function Augmented Expected Improvement
#'
#' @description
#' Expected Improvement.
#'
#' @export
AcqFunctionAEI = R6Class("AcqFunctionAEI",
  inherit = AcqFunction,
  public = list(

    #' @field y_effective_best (`numeric()`).
    y_effective_best = NULL,
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
    #' @param xdt [data.table::data.table]
    #'
    #' @return `data.table`
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
      xdt = archive$data()[, archive$cols_x]
      p = self$surrogate$predict(xdt)
      if (self$surrogate_max_to_min == 1) { # minimization
        y_effective = p$mean + self$param_set$values$c * p$se # pessimistic prediction
        self$y_effective_best = min(y_effective) 
      } else { # maximization
        y_effective = p$mean - self$param_set$values$c * p$se # pessimistic prediction
        self$y_effective_best = max(y_effective) 
      }
      self$noise_var = self$surrogate$model$model@covariance@nugget #FIXME: Check that this value really exists (otherwise calculate residual variance?)
    }
  )
)
