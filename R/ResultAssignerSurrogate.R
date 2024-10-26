#' @title Result Assigner Based on a Surrogate Mean Prediction
#'
#' @include ResultAssigner.R
#' @name mlr_result_assigners_surrogate
#'
#' @description
#' Result assigner that chooses the final point(s) based on a surrogate mean prediction of all evaluated points in the [bbotk::Archive].
#' This is especially useful in the case of noisy objective functions.
#'
#' In the case of operating on an [bbotk::OptimInstanceBatchMultiCrit] the [SurrogateLearnerCollection] must use as many learners as there are objective functions.
#'
#' @family Result Assigner
#' @export
#' @examples
#' result_assigner = ras("surrogate")
ResultAssignerSurrogate = R6Class("ResultAssignerSurrogate",
  inherit = ResultAssigner,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([Surrogate] | `NULL`)\cr
    #'   The surrogate that is used to predict the mean of all evaluated points.
    initialize = function(surrogate = NULL) {
      private$.surrogate = assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      super$initialize(label = "Mean Surrogate Prediction", man = "mlr3mbo::mlr_result_assigners_surrogate")
    },

    #' @description
    #' Assigns the result, i.e., the final point(s) to the instance.
    #' If `$surrogate` is `NULL`, `default_surrogate(instance)` is used and also assigned to `$surrogate`.
    #'
    #' @param instance ([bbotk::OptimInstanceBatchSingleCrit] | [bbotk::OptimInstanceBatchMultiCrit])\cr
    #'   The [bbotk::OptimInstance] the final result should be assigned to.
    assign_result = function(instance) {
      if (is.null(self$surrogate)) {
        self$surrogate = default_surrogate(instance)
      }
      if (inherits(instance, "OptimInstanceBatchSingleCrit")) {
        assert_r6(self$surrogate, classes = "SurrogateLearner")
      } else if (inherits(instance, "OptimInstanceBatchMultiCrit")) {
        assert_r6(self$surrogate, classes = "SurrogateLearnerCollection")
        if (self$surrogate$n_learner != instance$objective$ydim) {
          stopf("Surrogate used within the result assigner uses %i learners but the optimization instance has %i objective functions", self$surrogate$n_learner, instance$objective$ydim)
        }
      }

      archive = instance$archive
      self$surrogate$archive = archive
      xdt = archive_x(archive)
      self$surrogate$update()
      preds = self$surrogate$predict(xdt)
      means = if (inherits(self$surrogate, "SurrogateLearner")) {
        preds$mean
      } else if (inherits(self$surrogate, "SurrogateLearnerCollection")) {
        map_dtc(preds, "mean")
      }
      archive_tmp = archive$clone(deep = TRUE)
      archive_tmp$data[, self$surrogate$cols_y := means]
      xydt = archive_tmp$best()
      extra = xydt[, !c(archive_tmp$cols_x, archive_tmp$cols_y), with = FALSE]
      best = xydt[, archive_tmp$cols_x, with = FALSE]

      # ys are still the ones originally evaluated
      best_y = if (inherits(instance, "OptimInstanceBatchSingleCrit")) {
        unlist(archive$data[best, on = archive$cols_x][, archive$cols_y, with = FALSE])
      } else if (inherits(instance, "OptimInstanceBatchMultiCrit")) {
        archive$data[best, on = archive$cols_x][, archive$cols_y, with = FALSE]
      }
      instance$assign_result(xdt = best, best_y, extra = extra)
    }
  ),

  active = list(
    #' @template field_surrogate
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        private$.surrogate = assert_r6(rhs, classes = "Surrogate", null.ok = TRUE)
      }
    },

    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
    packages = function(rhs) {
      if (missing(rhs)) {
        if (is.null(self$surrogate)) character(0) else self$surrogate$packages
      } else {
        stop("$packages is read-only.")
      }
    }
  ),

  private = list(
    .surrogate = NULL,

    deep_clone = function(name, value) {
      switch(name,
        .surrogate = {if (!is.null(value)) value$clone(deep = TRUE) else value},
        value
      )
    }
  )
)

mlr_result_assigners$add("surrogate", ResultAssignerSurrogate)

