#' @title Acquisition function: SMS EMOA
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' @section Fields: See [AcqFunction]
#' @section Methods: See [AcqFunction]
#' @export
AcqFunctionSmsEmoa = R6Class("AcqFunctionSmsEmoa",
  inherit = AcqFunction, #FIXME: AcqFunctionMultiCrit?
  public = list(

    initialize = function(surrogate) { # FIXME: If we have a multi-output learner we might only want to use this learner as a single surrogate, alternatively we might want to have a SurrogateMulti class that abstracts this idea and allows both (a MultiOutput learner or multiple single learners)
      param_set = ParamSet$new(list(
        ParamDbl$new("lambda", lower = 0, default = 1)
        ParamDbl$new("eps", lower = 0, default = NULL) # for NULL, it will be calculated dynamically
      ))
      param_set$values$lambda = 1
      assert_list(surrogate, types = "SurrogateMultiCrit")
      super$initialize("acq_sms", param_set, surrogate = surrogate, direction = "maximize")
    },

    setup = function(archive) {
      super$setup(archive)

      n_obj = archive$codomain$length
      ys = archive$data()[, archive$y_cols, with = FALSE]
      ys = as.matrix(ys) %*% diag(self$mult_max_to_min)
      minimize = rep(TRUE, n_obj)
      self$ys_front = getNonDominatedPoints(ys, minimize = minimize)
      self$ref_point = getMultiObjRefPoint(ys, minimize = minimize) #FIXME: We should allow a fixed value, other options are worst seen + offset = 1

      if (is.null(self$param_set$values$eps)) {
        c_val = 1 - 1 / 2^n_obj
        eps = map_dbl(
          seq_col(ys_front),
          function(i) {
            (max(ys_front[, i]) - min(ys_front[, i])) /
            (ncol(ys_front) + c_val * (self$instance$terminator$param_set$values$n_evals - archive$n_evals)) #FIXME: Need progress here!
          }
        )
        self$eps = eps
      } else {
        self$eps = self$param_set$values$eps
      }
    },

    eval_dt = function(xdt) {
      ps = self$surrogate$predict(xdt)
      means = map_dtc(ps, "mean")
      ses = map_dtc(ps, "se")
      cbs = as.matrix(means) %*% diag(self$mult_max_to_min) - self$param_set$values$lambda * as.matrix(ses)
      # allocate mem for adding points to front for HV calculation in C
      front2 = t(rbind(self$ys_front, 0))
      sms = .Call("c_sms_indicator", PACKAGE = "mlr3mbo", cbs, self$ys_front, front2, self$eps, self$ref_point) #FIXME: Copy from mlrMBO
      data.table(acq_sms = sms)
    },

    update = function(archive) {
      super$update(archive)
    }
  )
)
