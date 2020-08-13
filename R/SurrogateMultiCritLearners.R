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
        if("se" %in% model$predict_types) {
          model$predict_type = "se"
        }  
      }
    },

    update = function(xydt, y_cols) {
      tasks = lapply(y_cols, function(y_col) {
        TaskRegr$new(
          id = "surrogate_task", 
          backend = xydt[, c(setdiff(colnames(xydt), y_cols), y_col), with = FALSE], 
          target = y_col)
      })
      mapply(function(model, task) model$train(task), self$model, tasks)
      names(self$model) = y_cols
      invisible(NULL)
    },

    predict = function(xdt) {
      preds = lapply(self$model, function(model) {
        pred = model$predict_newdata(newdata = xdt)
        if(model$predict_type == "se") {
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

