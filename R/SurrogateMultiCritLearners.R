#' @title Surrogate Model for MultiCriteria response surfaces
#'
#' @description
#' Multi Criteria response surfaces modeled by multiple mlr3 regression learners
#'
#' @export
SurrogateMultiCritLearners = R6Class("SurrogateMultiCritLearners",
  inherit = SurrogateMultiCrit,

  public = list(

    initialize = function(learners) {
      self$model = assert_learners(learners)
      for (model in self$model) {
        if("se" %in% self$model$predict_types) {
          model$predict_type = "se"
        }  
      }
    },

    update = function(xydt, y_cols) {
      tasks = lapply(y_cols, function(y_col) TaskRegr$new(id = "surrogate_task", backend = xydt, target = y_col))
      mapply(function(model, task) model$train(task), self$model)
      names(self$model) = y_cols
      invisible(NULL)
    },

    predict = function(xdt) {
      preds = lapply(self$model, function(model) {
        pred = self$model$predict_newdata(newdata = xdt)
        if(self$model$predict_type == "se") {
          data.table(mean = pred$response, se = pred$se)
        } else {
          data.table(mean = pred$response)
        }  
      })
      names(preds) = names(self$model)
      return(preds)
    }
  )

)

