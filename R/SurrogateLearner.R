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
#' }
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   xdt = generate_design_random(instance$search_space, n = 4)$data
#'
#'   instance$eval_batch(xdt)
#'
#'   learner = default_gp()
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   surrogate$update()
#'
#'   surrogate$learner$model
#' }
SurrogateLearner = R6Class("SurrogateLearner",
  inherit = Surrogate,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr]).
    #' @template param_archive_surrogate
    #' @template param_col_y_surrogate
    #' @template param_cols_x_surrogate
    initialize = function(learner, archive = NULL, cols_x = NULL, col_y = NULL) {
      assert_learner(learner)
      if (learner$predict_type != "se" && "se" %in% learner$predict_types) {
        learner$predict_type = "se"
      }

      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_character(cols_x, min.len = 1L, null.ok = TRUE)
      assert_string(col_y, null.ok = TRUE)

      ps = ParamSet$new(list(
        ParamLgl$new("assert_insample_perf"),
        ParamUty$new("perf_measure", custom_check = function(x) check_r6(x, classes = "MeasureRegr")),  # FIXME: actually want check_measure
        ParamDbl$new("perf_threshold", lower = -Inf, upper = Inf),
        ParamLgl$new("catch_errors"),
        ParamFct$new("impute_missings", levels = c("none", "mean", "random")))
      )
      ps$values = list(assert_insample_perf = FALSE, catch_errors = TRUE, impute_missings = "none")
      ps$add_dep("perf_measure", on = "assert_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_threshold", on = "assert_insample_perf", cond = CondEqual$new(TRUE))

      super$initialize(learner = learner, archive = archive, cols_x = cols_x, cols_y = col_y, param_set = ps)
    },

    #' @description
    #' Predict mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)
      xdt = fix_xdt_missing(xdt, cols_x = self$cols_x, archive = self$archive)

      pred = self$learner$predict_newdata(newdata = xdt)
      if (self$learner$predict_type == "se") {
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
        class(self$learner)[1L]
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
      if (missing(rhs)) {
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
      } else {
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
        self$learner$packages
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        self$learner$feature_types
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @template field_properties_surrogate
    properties = function(rhs) {
      if (missing(rhs)) {
        self$learner$properties
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_predict_type_surrogate
    predict_type = function(rhs) {
      if (missing(rhs)) {
        self$learner$predict_type
      } else {
        stop("$predict_type is read-only. To change it, modify $predict_type of the learner directly.")
      }
    }
  ),

  private = list(
    # Train learner with new data.
    # Also calculates the insample performance based on the `perf_measure` hyperparameter if `assert_insample_perf = TRUE`.
    .update = function() {
      if (self$param_set$values$impute_missings == "mean") {
        xydt = self$archive$rush$fetch_tasks_with_state(states = c("queued", "running", "finished"))[, c(self$cols_x, self$cols_y), with = FALSE]
        setnafill(xydt, type = "const", fill = mean(xydt[[self$cols_y]], na.rm = TRUE), cols = self$cols_y)
      } else if (self$param_set$values$impute_missings == "random") {
        xydt = self$archive$rush$fetch_tasks_with_state(states = c("queued", "running", "finished"))[, c(self$cols_x, self$cols_y, "state"), with = FALSE]
        walk(self$cols_y, function(col) {
          min = min(xydt[[col]], na.rm = TRUE)
          max = max(xydt[[col]], na.rm = TRUE)
          xydt[state %in% c("queued", "running"), (col) := runif(.N, min, max)]
        })
        set(xydt, j = "state", value = NULL)
      } else {
        xydt = self$archive$data[, c(self$cols_x, self$cols_y), with = FALSE]
      }

      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = self$cols_y)
      assert_learnable(task, learner = self$learner)
      self$learner$train(task)

      if (self$param_set$values$assert_insample_perf) {
        measure = assert_measure(self$param_set$values$perf_measure %??% mlr_measures$get("regr.rsq"), task = task, learner = self$learner)
        private$.insample_perf = self$learner$predict(task)$score(measure, task = task, learner = self$learner)
        self$assert_insample_perf
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        learner = value$clone(deep = TRUE),
        .param_set = value$clone(deep = TRUE),
        .archive = value$clone(deep = TRUE),
        value
      )
    }
  )
)

