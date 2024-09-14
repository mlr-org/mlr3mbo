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
#'   the failed acquisition function optimization (as a result of the failed surrogate) appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Default is `TRUE`.
#' }
#' \item{`impute_method`}{`character(1)`\cr
#'   Method to impute missing values in the surrogate model.
#' }
#' }
#'
#' @export
SurrogateLearnerAsync = R6Class("SurrogateLearnerAsync",
  inherit = SurrogateLearner,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr]).
    #' @template param_archive_surrogate
    #' @template param_col_y_surrogate
    #' @template param_cols_x_surrogate
    initialize = function(learner, archive = NULL, cols_x = NULL, col_y = NULL) {
      super$initialize(learner = learner, archive = archive, cols_x = cols_x, col_y = col_y)

      extra_ps = ps(
        impute_method = p_fct(c("mean", "random"), default = "random")
      )

      private$.param_set = c(private$.param_set, extra_ps)
      private$.param_set$set_values(impute_method = "random")
    }
  ),

  private = list(
    .update = function() {
      xydt = self$archive$rush$fetch_tasks_with_state(states = c("queued", "running", "finished"))[, c(self$cols_x, self$cols_y, "state"), with = FALSE]


      if (self$param_set$values$impute_method == "mean") {
        setnafill(xydt, type = "const", fill = mean(xydt[[self$cols_y]], na.rm = TRUE), cols = self$cols_y)
      } else if (self$param_set$values$impute_method == "random") {
        walk(self$cols_y, function(col) {
          min = min(xydt[[col]], na.rm = TRUE)
          max = max(xydt[[col]], na.rm = TRUE)
          xydt[c("queued", "running"), (col) := runif(.N, min, max), on = "state"]
        })
      }
      set(xydt, j = "state", value = NULL)

      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = self$cols_y)
      assert_learnable(task, learner = self$learner)
      self$learner$train(task)

      if (self$param_set$values$assert_insample_perf) {
        measure = assert_measure(self$param_set$values$perf_measure %??% mlr_measures$get("regr.rsq"), task = task, learner = self$learner)
        private$.insample_perf = self$learner$predict(task)$score(measure, task = task, learner = self$learner)
        self$assert_insample_perf
      }
    }
  )
)

