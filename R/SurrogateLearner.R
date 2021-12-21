#' @title Surrogate Model Containing a Single Learner
#'
#' @description
#' Surrogate model containing a single regression [mlr3::Learner].
#'
#' @export
SurrogateLearner = R6Class("SurrogateLearner",
  inherit = Surrogate,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr]).
    #' @param archive (`NULL` | [bbotk::Archive]).
    #' @param y_col (`NULL` | `character(1)`).
    initialize = function(learner, archive = NULL, y_col = NULL) {
      # FIXME: deep clone learner?
      assert_learner(learner)
      if (learner$predict_type != "se" && "se" %in% learner$predict_types) {
        learner$predict_type = "se"
      }

      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_string(y_col, null.ok = TRUE)

      ps = ParamSet$new(list(
        ParamLgl$new("calc_insample_perf"),
        ParamUty$new("perf_measure", custom_check = function(x) check_r6(x, classes = "MeasureRegr")),  # FIXME: actually want check_measure
        ParamDbl$new("perf_threshold", lower = -Inf, upper = Inf))
      )
      ps$values = list(calc_insample_perf = FALSE, perf_measure = msr("regr.rsq"), perf_threshold = 0)
      ps$add_dep("perf_measure", on = "calc_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_threshold", on = "calc_insample_perf", cond = CondEqual$new(TRUE))

      super$initialize(model = learner, archive = archive, y_cols = y_col, param_set = ps)
    },

    #' @description
    #' Returns mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)

      # FIXME: within acquisition function optimization instance$eval_batch() may drop NA
      # https://github.com/mlr-org/bbotk/blob/1559ea9fd1d60816ad5c375984134d6445bf5867/R/OptimInstance.R#L141
      # because they way transform_xdt_to_xss works
      # we fix this here and readd those columns with NA again
      # because a learner expects consistency between train and predict task
      cols_to_add = self$archive$cols_x[self$archive$cols_x %nin% colnames(xdt)]
      if (length(cols_to_add)) {
        xdt[, (cols_to_add) := NA]
      }

      pred = self$model$predict_newdata(newdata = char_to_fct(xdt))
      if (self$model$predict_type == "se") {
        data.table(mean = pred$response, se = pred$se)
      } else {
        data.table(mean = pred$response)
      }
    }
  ),

  active = list(

    #' @field n_learner (`integer(1)`)\cr
    #'   Returns the number of [mlr3::Learner]s.
    n_learner = function() {
      1L
    },

    #' @field assert_insample_perf (`numeric(1)`)\cr
    #'   Asserts whether the current insample performance meets the performance threshold.
    assert_insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stop("assert_insample_perf is read-only.")
      }

      if (!self$param_set$values$calc_insample_perf) {
        return(invisible(self$insample_perf))
      }

      check = if (self$param_set$values$perf_measure$minimize) {
        self$insample_perf < self$param_set$values$perf_threshold
      } else {
        self$insample_perf > self$param_set$values$perf_threshold
      }

      if (!check) {
        stop("Current insample performance of the Surrogate Model does not meet the performance threshold.")
      }
      invisible(self$insample_perf)
    }
  ),

  private = list(

    # Train model with new data.
    # Also calculates the insample performance based on the `perf_measure` hyperparameter if `calc_insample_perf = TRUE`.
    .update = function() {
      xydt = self$archive$data[, c(self$archive$cols_x, self$y_cols), with = FALSE]
      task = TaskRegr$new(id = "surrogate_task", backend = char_to_fct(xydt), target = self$y_cols)
      assert_learnable(task, learner = self$model)
      self$model$train(task)

      if (self$param_set$values$calc_insample_perf) {
        assert_measure(self$param_set$values$perf_measure, task = task, learner = self$model)
        private$.insample_perf = self$model$predict(task)$score(self$param_set$values$perf_measure, task = task, learner = self$model)
        self$assert_insample_perf
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        model = value$clone(deep = TRUE),
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
