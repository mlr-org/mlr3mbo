#' @title Acquisition Function SMS EGO
#'
#' @description
#' S-Metric Selection Evolutionary Multiobjective Optimization Algorithm.
#'
#' @section Parameters:
#' * `"lambda"` (`numeric(1)`)\cr
#'   \eqn{\lambda} value used for the confidence bound.
#'   Defaults to 1.
#'   Based on `confidence = (1 - 2 * dnorm(lambda)) ^ m` you can calculate a
#'   lambda for a given confidence level, see Ponweiser et al. 2008.
#' * `"eps"` (`numeric(1)`)\cr
#'   \eqn{\epsilon} used for the additive epsilon dominance.
#'   Can either be a single numeric value > 0 or `NULL`.
#'   In the case of being `NULL`, an epsilon vector is maintained dynamically as
#'   described in Ponweiser et al. 2008.
#'
#' @references
#' `r format_bib("ponweiser_2008")`
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionSmsEgo = R6Class("AcqFunctionSmsEgo",
  inherit = AcqFunction,

  public = list(

    #' @field ys_front (`matrix()`).
    ys_front = NULL,

    #' @field ref_point (`numeric()`).
    ref_point = NULL,

    #' @field eps (`numeric()`).
    eps = NULL,

    # FIXME:
    #' @field progress (`numeric(1)`).
    progress = NULL,

    # FIXME: also in super class? private?
    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateMultiCrit]).
    initialize = function(surrogate) {
      param_set = ParamSet$new(list(
        ParamDbl$new("lambda", lower = 0, default = 1),
        ParamDbl$new("eps", lower = 0, default = NULL, special_vals = list(NULL))  # for NULL, it will be calculated dynamically
      ))
      param_set$values$lambda = 1
      self$param_set = param_set

      assert_r6(surrogate, "SurrogateMultiCrit")

      fun = function(xdt) {
        ps = self$surrogate$predict(xdt)
        means = map_dtc(ps, "mean")
        ses = map_dtc(ps, "se")
        cbs = as.matrix(means) %*% diag(self$surrogate_max_to_min) - self$param_set$values$lambda * as.matrix(ses)
        # allocate memory for adding points to front for HV calculation in C
        front2 = t(rbind(self$ys_front, 0))
        sms = .Call("c_sms_indicator", PACKAGE = "mlr3mbo", cbs, self$ys_front, front2, self$eps, self$ref_point)
        data.table(acq_sms = sms)
      }

      super$initialize("acq_sms", surrogate = surrogate, direction = "minimize", fun = fun)
    },

    #' @description
    #' Updates acquisition function and sets `$y_best`.
    #'
    # FIXME: should use instance and not archive?
    #' @param archive ([bbotk::Archive]).
    update = function(archive) {
      super$update(archive)

      n_obj = archive$codomain$length
      ys = archive$data[, archive$cols_y, with = FALSE]
      ys = as.matrix(ys) %*% diag(self$surrogate_max_to_min)
      self$ys_front = as.matrix(archive$best()[, archive$cols_y, with = FALSE]) %*% diag(self$surrogate_max_to_min)
      self$ref_point = apply(ys, MARGIN = 2L, FUN = max) + 1  # offset = 1 like in mlrMBO

      if (is.null(self$param_set$values$eps)) {
        c_val = 1 - 1 / 2^n_obj
        eps = map_dbl(
          seq_col(self$ys_front),
          function(i) {
            (max(self$ys_front[, i]) - min(self$ys_front[, i])) /
            (ncol(self$ys_front) + c_val * self$progress)  # FIXME: Need self$progress here! Originally self$instance$terminator$param_set$values$n_evals - archive$n_evals
          }
        )
        self$eps = eps
      } else {
        self$eps = self$param_set$values$eps
      }
    }
  )
)
