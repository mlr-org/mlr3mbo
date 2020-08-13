#' @title Surrogate Model from mlr3 Learner
#'
#' @description
#'
#' @export
SurrogateSingleCritLearner = R6Class("SurrogateSingleCritLearner",
  inherit = SurrogateSingleCrit,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner [mlr3::LearnerRegr]
    initialize = function(learner) {
      self$model = assert_learner(learner)
      if("se" %in% self$model$predict_types) {
        self$model$predict_type = "se"
      }
    },

    #' @description
    #' Train model with new points.
    #'
    update = function(xydt, y_cols) {
      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = y_cols)
      self$model$train(task)
    },

    #' @description
    #' Returns mean response and standard error
    #'
    #' @return [data.table::data.table]
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
