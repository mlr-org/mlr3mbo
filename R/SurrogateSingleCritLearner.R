#' @title Surrogate Model
#'
#' @description
#' Surrogate model based on regression [mlr3::Learner] objects.
#'
#' @export
SurrogateSingleCritLearner = R6Class("SurrogateSingleCritLearner",
  inherit = SurrogateSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner [mlr3::LearnerRegr].
    initialize = function(learner) {
      self$model = assert_learner(learner)
      if (self$model$predict_type != "se" && "se" %in% self$model$predict_types) {
        self$model$predict_type = "se"
      }

      private$.param_set = ParamSet$new(list(
        #ParamLgl$new("calc_insample_perf", default = TRUE),  # FIXME: do we actually want this to be turned off?
        ParamUty$new("perf_measure", custom_check = function(x) check_r6(x, classes = "MeasureRegr")),  # FIXME: actually want check_measure
        ParamDbl$new("perf_threshold", lower = -Inf, upper = Inf))
      )
      #private$.param_set$values$calc_insample_perf = TRUE
      private$.param_set$values$perf_measure = msr("regr.rsq")
      private$.param_set$values$perf_threshold = 0
    },

    #' @description
    #' Train model with new points.
    #' Also calculates the insample performance based on the `perf_measure` hyperparameter.
    #'
    #' @param xydt [data.table::data.table]\cr
    #' Desing of new points.
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
    #'
    #' @return `NULL`
    update = function(xydt, y_cols) {
      assert_xydt(xydt, y_cols)
      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = y_cols)
      self$model$train(task)

      #if (self$param_set$values$calc_insample_perf) {
      assert_measure(self$param_set$values$perf_measure, task = task, learner = self$model)
      private$.insample_performance = self$model$predict(task)$score(self$param_set$values$perf_measure, task = task, learner = self$model)
      #}

      invisible(NULL)
    },

    #' @description
    #' Returns mean response and standard error.
    #'
    #' @param xdt [data.table::data.table()]\cr
    #' New data.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)

      pred = self$model$predict_newdata(newdata = xdt)
      if (self$model$predict_type == "se") {
        data.table(mean = pred$response, se = pred$se)
      } else {
        data.table(mean = pred$response)
      }
    }
  ),

  active = list(

    #' @field assert_insample_performance (`logical(1)`)\cr
    #' Whether the current insample performance meets the `perf_threshold`.
    assert_insample_performance = function(rhs) {  # FIXME: better name
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only")
      }

      check = if (self$param_set$values$perf_measure$minimize) {
        self$insample_performance < self$param_set$values$perf_threshold
      } else {
        self$insample_performance > self$param_set$values$perf_threshold
      }

      if (!check) {
        stopf("Current insample performance of the Surrogate Model does not meet the performance threshold")
      }
      invisible(self$insample_performance)
    }
  )
)
