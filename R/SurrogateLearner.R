#' @title Surrogate Model Containing a Single Learner
#'
#' @description
#' Surrogate model containing a single [mlr3::LearnerRegr].
#'
#' @section Parameters:
#' \describe{
#' \item{`assert_insample_perf`}{`logical(1)`\cr
#'   Should the insample performance of the [mlr3::LearnerRegr] be asserted after updating the surrogate?
#'   If the assertion fails (i.e., the insample performance based on the `perf_measure` does not meet the
#'   `perf_threshold`), an error is thrown.
#'   Default is `FALSE`.
#' }
#' \item{`perf_measure`}{[mlr3::MeasureRegr]\cr
#'   Performance measure which should be use to assert the insample performance of the [mlr3::LearnerRegr].
#'   Only relevant if `assert_insample_perf = TRUE`.
#'   Default is [mlr3::mlr_measures_regr.rsq].
#' }
#' \item{`perf_threshold`}{`numeric(1)`\cr
#'   Threshold the insample performance of the [mlr3::LearnerRegr] should be asserted against.
#'   Only relevant if `assert_insample_perf = TRUE`.
#'   Default is `0`.
#' }
#' \item{`catch_errors`}{`logical(1)`\cr
#'   Should errors during updating the surrogate be caught and propagated to the `loop_function` which can then handle
#'   the failed infill optimization (as a result of the failed surrogate) appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Default is `TRUE`.
#' }
#' }
#'
#' @export
SurrogateLearner = R6Class("SurrogateLearner",
  inherit = Surrogate,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr]).
    #' @template param_archive_surrogate
    #' @template param_x_cols_surrogate
    #' @template param_y_col_surrogate
    initialize = function(learner, archive = NULL, x_cols = NULL, y_col = NULL) {
      assert_learner(learner)
      if (learner$predict_type != "se" && "se" %in% learner$predict_types) {
        learner$predict_type = "se"
      }

      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_character(x_cols, min.len = 1L, null.ok = TRUE)
      assert_string(y_col, null.ok = TRUE)

      ps = ParamSet$new(list(
        ParamLgl$new("assert_insample_perf"),
        ParamUty$new("perf_measure", custom_check = function(x) check_r6(x, classes = "MeasureRegr")),  # FIXME: actually want check_measure
        ParamDbl$new("perf_threshold", lower = -Inf, upper = Inf),
        ParamLgl$new("catch_errors"))
      )
      ps$values = list(assert_insample_perf = FALSE, catch_errors = TRUE)
      ps$add_dep("perf_measure", on = "assert_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_threshold", on = "assert_insample_perf", cond = CondEqual$new(TRUE))

      super$initialize(model = learner, archive = archive, x_cols = x_cols, y_cols = y_col, param_set = ps)
    },

    #' @description
    #' Returns mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)
      xdt = fix_xdt_missing(xdt, x_cols = self$x_cols, archive = self$archive)
      xdt = char_to_fct(xdt)

      pred = self$model$predict_newdata(newdata = xdt)
      if (self$model$predict_type == "se") {
        data.table(mean = pred$response, se = pred$se)
      } else {
        data.table(mean = pred$response)
      }
    }
  ),

  active = list(

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        class(self$model)[1L]
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @template field_n_learner_surrogate
    n_learner = function() {
      1L
    },

    #' @template field_assert_insample_perf_surrogate
    assert_insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stop("$assert_insample_perf is read-only.")
      }

      if (!self$param_set$values$assert_insample_perf) {
        return(invisible(self$insample_perf))
      }

      perf_measure = self$param_set$values$perf_measure %??% mlr_measures$get("regr.rsq")
      perf_threshold = self$param_set$values$perf_threshold %??% 0
      check = if (perf_measure$minimize) {
        self$insample_perf < perf_threshold
      } else {
        self$insample_perf > perf_threshold
      }

      if (!check) {
        stop("Current insample performance of the Surrogate Model does not meet the performance threshold.")
      }
      invisible(self$insample_perf)
    },

    #' @template field_packages_surrogate
    packages = function(rhs) {
      if (missing(rhs)) {
        self$model$packages
      } else {
        stop("$packages is read-only.")
      }

    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        self$model$feature_types
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @field properties (`character()`)\cr
    #' Stores a set of properties/capabilities the learner has.
    #' A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
    properties = function(rhs) {
      if (missing(rhs)) {
        self$model$properties
      } else {
        stop("$properties is read-only.")
      }
    }
  ),

  private = list(
    # Train model with new data.
    # Also calculates the insample performance based on the `perf_measure` hyperparameter if `assert_insample_perf = TRUE`.
    .update = function() {
      xydt = self$archive$data[, c(self$x_cols, self$y_cols), with = FALSE]
      task = TaskRegr$new(id = "surrogate_task", backend = char_to_fct(xydt), target = self$y_cols)
      assert_learnable(task, learner = self$model)
      self$model$train(task)

      if (self$param_set$values$assert_insample_perf) {
        measure = assert_measure(self$param_set$values$perf_measure %??% mlr_measures$get("regr.rsq"), task = task, learner = self$model)
        private$.insample_perf = self$model$predict(task)$score(measure, task = task, learner = self$model)
        self$assert_insample_perf
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        model = value$clone(deep = TRUE),
        .param_set = value$clone(deep = TRUE),
        .archive = value$clone(deep = TRUE),
        value
      )
    }
  )
)
