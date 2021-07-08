#' @title Surrogate Model for MultiCriteria response surfaces
#'
#' @description
#' Multi Criteria response surfaces modeled by multiple mlr3 regression learners
#'
#' @export
SurrogateMultiCritLearners = R6Class("SurrogateMultiCritLearners",
  inherit = SurrogateMultiCrit,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (list of [mlr3::LearnerRegr]).
    initialize = function(learners) {
      self$model = assert_learners(learners)
      for (model in self$model) {
        if (model$predict_type != "se" && "se" %in% model$predict_types) {
          model$predict_type = "se"
        }
      }
    },

    #' @description
    #' Train model with new points.
    #'
    #' @param xydt ([data.table::data.table()]).
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
    update = function(xydt, y_cols) {
      assert_xydt(xydt, y_cols)


      backend = as_data_backend(char_to_fct(xydt))
      features = setdiff(names(xydt), y_cols)

      tasks = lapply(y_cols, function(y_col) {
        # If this turns out to be a bottleneck, we can also operate on a
        # single task here
        task = TaskRegr$new(
          id = paste0("surrogate_task_", y_col),
          backend = backend,
          target = y_col)
        task$col_roles$feature = features
        task
      })
      mapply(function(model, task) model$train(task), self$model, tasks)
      names(self$model) = y_cols
      invisible(NULL)
    },

    #' @description
    #' Returns mean response and standard error
    #'
    #' @param xdt [data.table::data.table()]\cr
    #' New data.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)

      preds = lapply(self$model, function(model) {
        pred = model$predict_newdata(newdata = xdt)
        if (model$predict_type == "se") {
          data.table(mean = pred$response, se = pred$se)
        } else {
          data.table(mean = pred$response)
        }
      })
      names(preds) = names(self$model)
      return(preds)
    }
  ),

  active = list(
    #' @field k
    #' Returns the number of models.
    k = function() {
      length(self$model)
    }
  )
)
