#' @title Surrogate Model for Multi Criteria Response Surfaces
#'
#' @description
#' Surrogate model for multi criteria response surfaces modeled by multiple regression [mlr3::Learner] objects.
#' Note that redundant [mlr3::Learner]s must be deep clones.
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
      addresses = map(learners, address)
      if (length(unique(addresses)) != length(addresses)) {
        stop("Redundant Learners must be unique in memory, i.e., deep clones.")
      }
      self$model = assert_learners(learners)
      for (model in self$model) {
        if (model$predict_type != "se" && "se" %in% model$predict_types) {
          model$predict_type = "se"
        }
      }
      ps = ParamSet$new(list(
        ParamLgl$new("calc_insample_perf"),
        ParamUty$new("perf_measures", custom_check = function(x) check_list(x, types = "MeasureRegr", any.missing = FALSE, len = length(self$model))),  # FIXME: actually want check_measures
        ParamUty$new("perf_thresholds", custom_check = function(x) check_double(x, lower = -Inf, upper = Inf, any.missing = FALSE, len = length(self$model))))
      )
      ps$values = list(calc_insample_perf = FALSE, perf_measures = replicate(length(self$model), msr("regr.rsq")), perf_thresholds = rep(0, length(self$model)))
      ps$add_dep("perf_measures", on = "calc_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_thresholds", on = "calc_insample_perf", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one dimension.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data.
    #'
    #' @return list of [data.table::data.table()]s with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)

      preds = lapply(self$model, function(model) {
        pred = model$predict_newdata(newdata = char_to_fct(xdt))
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

    #' @field k (`integer(1)`)\cr
    #' Returns the number of models.
    k = function() {
      length(self$model)
    },

    #' @field assert_insample_perf (`numeric()`)\cr
    #' Asserts whether the current insample performance meets the performance threshold.
    assert_insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stop("Field/Binding is read-only.")
      }

      if (!self$param_set$values$calc_insample_perf) {
        return(invisible(self$insample_perf))
      }

      check = all(pmap_lgl(
        list(
          insample_perf = self$insample_perf,
          perf_threshold = self$param_set$values$perf_thresholds,
          perf_measure = self$param_set$values$perf_measures
        ),
        .f = function(insample_perf, perf_threshold, perf_measure) {
          if (perf_measure$minimize) {
            insample_perf < perf_threshold
          } else {
            insample_perf > perf_threshold
          }
        })
      )

      if (!check) {
        stop("Current insample performance of the Surrogate Model does not meet the performance threshold")
      }
      invisible(self$insample_perf)
    }
  ),

  private = list(

    # Train model with new data
    # Also calculates the insample performance based on the `perf_measures` hyperparameter if `calc_insample_perf = TRUE`
    .update = function(xydt, y_cols) {
      assert_xydt(xydt, y_cols)

      backend = as_data_backend(char_to_fct(xydt))
      features = setdiff(names(xydt), y_cols)

      tasks = lapply(y_cols, function(y_col) {
        # If this turns out to be a bottleneck, we can also operate on a single task here
        task = TaskRegr$new(
          id = paste0("surrogate_task_", y_col),
          backend = backend,
          target = y_col)
        task$col_roles$feature = features
        task
      })
      pmap(list(model = self$model, task = tasks), .f = function(model, task) {
        assert_learnable(task, learner = model)
        model$train(task)
        invisible(NULL)
      })
      names(self$model) = y_cols

      if (self$param_set$values$calc_insample_perf) {
        private$.insample_perf = setNames(pmap_dbl(list(model = self$model, task = tasks, perf_measure = self$param_set$values$perf_measures),
          .f = function(model, task, perf_measure) {
            assert_measure(perf_measure, task = task, learner = model)
            model$predict(task)$score(perf_measure, task = task, learner = model)
          }
        ), nm = map_chr(self$param_set$values$perf_measures, "id"))
        self$assert_insample_perf
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        model = map(value, function(x) x$clone(deep = TRUE)),
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
