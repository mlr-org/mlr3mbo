#' @title Surrogate Model from mlr3 Learner
#'
#' @description
#'
#' @export
SurrogateLearner = R6Class("Surrogate",
  inherit = Surrogate,
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
    update = function(xdt, ydt) {
      data = cbind(xdt, ydt)
      task = TaskRegr$new("task", data, colnames(ydt))
      self$model$train(task)
    },

    #' @description
    #' Returns mean response and standard error
    #'
    #' @return [data.table::data.table]
    predict = function(xdt) {
      pred = self$model$predict_newdata(newdata = xdt)
      if(self$model$predict_type == "se") {
        data.table(y = pred$response, se = pred$se)
      } else {
        data.table(y = pred$response)
      }
    }
  )
)
