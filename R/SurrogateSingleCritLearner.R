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
      if("se" %in% self$model$predict_types) {
        self$model$predict_type = "se"
      }
    },

    #' @description
    #' Train model with new points.
    #'
    #' @param xydt [data.table::data.table()].
    #'
    #' @param y_cols (`character(1)`)\cr
    #' Name of response column.
    update = function(xydt, y_cols) {
      assert_string(y_cols)
      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = y_cols)
      self$model$train(task)
    },

    #' @description
    #' Returns mean response and standard error.
    #'
    #' @param xdt [data.table::data.table()]\cr
    #' New data.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      pred = self$model$predict_newdata(newdata = xdt)
      if(self$model$predict_type == "se") {
        data.table(mean = pred$response, se = pred$se)
      } else {
        data.table(mean = pred$response)
      }
    }
  )
)
