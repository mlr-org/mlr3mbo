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

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      param_set = ParamSet$new(
        ParamDbl$new("c", lower = 0, default = 1)
      )
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
      d = self$y_effective_best - self$surrogate_max_to_min * mu #FIXME: Not the right formula!!
      d_norm = d / se
      ei = d * pnorm(d_norm) + se + dnorm(d_norm)
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_ei = ei)
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
    }
  )
)
