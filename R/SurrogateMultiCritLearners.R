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

       private$.param_set = ParamSet$new(list(
        #ParamLgl$new("calculate_insample_performance", default = TRUE),  # FIXME: do we actually want this to be turned off?
        ParamUty$new("performance_measures", custom_check = function(x) check_list(x, types = "MeasureRegr", any.missing = FALSE, len = length(self$model))),  # FIXME: actually want check_measures
        ParamUty$new("performance_epsilons", custom_check = function(x) check_double(x, lower = -Inf, upper = Inf, any.missing = FALSE, len = length(self$model))))
      )
      # FIXME: adaptive defaults based on learner and measure?
      #private$.param_set$values$calculate_insample_performance = TRUE
      private$.param_set$values$performance_measures = replicate(length(self$model), msr("regr.rsq"))
      private$.param_set$values$performance_epsilons = rep(0, length(self$model))

    },

    #' @description
    #' Train model with new points.
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
      private$.insample_performance = if (all(map_lgl(acq_function$surrogate$model, function(x) !is.null(x$model)))) {
        mapply(function(model, task, performance_measure) {
          assert_measure(performance_measure, task = task, learner = model)
          model$predict(task)$score(performance_measure, task = task, learner = model)
        }, model = self$model, task = tasks, performance_measure = self$param_set$values$performance_measure)
      } else {
        NULL
      }
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
      return(preds)
    }
  ),

  active = list(
    #' @field k
    #' Returns the number of models.
    k = function() {
      length(self$model)
    },

    #' @description
    #' #FIXME:
    test_insample_performance = function(rhs) {  # FIXME: better name
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only")
      }

      all(mapply(function(insample_performance, performance_epsilon, performance_measure) {
        if (performance_measure$minimize) {
          insample_performance < performance_epsilon
        } else {
          insample_performance > performance_epsilon
        }
      }, insample_performance = self$insample_performance, performance_epsilon = self$param_set$values$performance_epsilons, performance_measure = self$param_set$values$performance_measures))

      #if (check_passed) {
      #  invisible(TRUE)
      #} else {
      #  stopf("Insample performance of the Surrogate Model does not meet the defined criterion.")
      #}
    }

  )
)
