#' @title Surrogate Model for MultiCriteria response surfaces
#'
#' @description
#' Multi Criteria response surfaces modeled by multiple regression [mlr3::Learner] objects.
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

       private$.param_set = ParamSet$new(list(
        #ParamLgl$new("calc_insample_perf", default = TRUE),  # FIXME: do we actually want this to be turned off?
        ParamUty$new("perf_measures", custom_check = function(x) check_list(x, types = "MeasureRegr", any.missing = FALSE, len = length(self$model))),  # FIXME: actually want check_measures
        ParamUty$new("perf_thresholds", custom_check = function(x) check_double(x, lower = -Inf, upper = Inf, any.missing = FALSE, len = length(self$model))))
      )
      #private$.param_set$values$calc_insample_perf = TRUE
      private$.param_set$values$perf_measures = replicate(length(self$model), msr("regr.rsq"))
      private$.param_set$values$perf_thresholds = rep(0, length(self$model))

    },

    #' @description
    #' Train model with new points.
    #' Also calculates the insample performance based on the `perf_measures` hyperparameter.
    #'
    #' @param xydt ([data.table::data.table()])\cr
    #' Desing of new points.
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
    #'
    #' @return `NULL`
    update = function(xydt, y_cols) {
      assert_xydt(xydt, y_cols)

      backend = as_data_backend(xydt)
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
      # FIXME: do we really want to use mapply? (see also below and in test_insample_performance)
      mapply(function(model, task) {
        model$train(task)
        NULL
      }, model = self$model, task = tasks)
      names(self$model) = y_cols

      # FIXME: see comments in SurrogateSingleCritLearner.R
      #if (self$param_set$values$calc_insample_perf) {
      private$.insample_performance = mapply(function(model, task, perf_measure) {
        assert_measure(perf_measure, task = task, learner = model)
        model$predict(task)$score(perf_measure, task = task, learner = model)
      }, model = self$model, task = tasks, perf_measure = self$param_set$values$perf_measures)
      #}

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

      preds = map(self$model, function(model) {
        pred = model$predict_newdata(newdata = xdt)
        if (model$predict_type == "se") {
          data.table(mean = pred$response, se = pred$se)
        } else {
          data.table(mean = pred$response)
        }
      })
      names(preds) = names(self$model)
      preds
    }
  ),

  active = list(

    #' @field k (`integer(1)`)\cr
    #' Returns the number of models.
    k = function() {
      length(self$model)
    },

    #' @field assert_insample_performance (`logical(1)`)\cr
    #' Whether the current insample performance meets the `perf_threshold`.
    assert_insample_performance = function(rhs) {  # FIXME: better name
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only")
      }

      check = all(mapply(function(insample_performance, perf_threshold, perf_measure) {
        if (perf_measure$minimize) {
          insample_performance < perf_threshold
        } else {
          insample_performance > perf_threshold
        }
      }, insample_performance = self$insample_performance, perf_threshold = self$param_set$values$perf_threshold, perf_measure = self$param_set$values$perf_measures))

      if (!check) {
        stopf("Current insample performance of the Surrogate Model does not meet the performance threshold")
      }
      invisible(self$insample_performance)
    }
  )
)
