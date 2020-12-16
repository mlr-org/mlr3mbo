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
        #ParamLgl$new("calculate_insample_performance", default = TRUE),  # FIXME: do we actually want this to be turned off?
        ParamUty$new("performance_measure", custom_check = function(x) check_r6(x, classes = "MeasureRegr")),  # FIXME: actually want check_measure
        ParamDbl$new("performance_epsilon", lower = -Inf, upper = Inf))
      )
      # FIXME: adaptive defaults based on learner and measure?
      #private$.param_set$values$calculate_insample_performance = TRUE
      private$.param_set$values$performance_measure = msr("regr.rsq")
      private$.param_set$values$performance_epsilon = 0
    },

    #' @description
    #' Train model with new points.
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
      #if (self$param_set$values$calculate_insample_performance) {
      # FIXME: this can likely go once mlr-org/mlr3mbo/issues/33 is fixed (if we encapsulate and provide a fallback we can always predict)
      # only update the insample performance if training was successful and we can actually predict from the Learner
      # otherwise return NULL, this will result in insufficient insample performance later and trigger a fix
      private$.insample_performance = if (!is.null(self$model$model)) {
        assert_measure(self$param_set$values$performance_measure, task = task, learner = self$model)
        # FIXME: this may need additional safety checks because the prediction may still fail for some weird reason
        self$model$predict(task)$score(self$param_set$values$performance_measure, task = task, learner = self$model)
      }  else {
        NULL
      }
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
    #' @description
    #' #FIXME:
    test_insample_performance = function(rhs) {  # FIXME: better name
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only")
      }

      if (self$param_set$values$performance_measure$minimize) {
        # FIXME: this is not numerically stable
        self$insample_performance < self$param_set$values$performance_epsilon
      } else {
        self$insample_performance > self$param_set$values$performance_epsilon
      }

      #if (check_passed) {
      #  invisible(TRUE)
      #} else {
      #  stopf("Insample performance of the Surrogate Model does not meet the defined criterion.")
      #}
    }
  )
)
