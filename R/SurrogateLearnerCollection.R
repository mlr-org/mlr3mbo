#' @title Surrogate Model Containing Multiple Learners
#'
#' @description
#' Surrogate model containing multiple [mlr3::LearnerRegr].
#' The [mlr3::LearnerRegr] are fit on the target variables as indicated via `y_cols`.
#' Note that redundant [mlr3::LearnerRegr] must be deep clones.
#'
#' @section Parameters:
#' \describe{
#' \item{`assert_insample_perf`}{`logical(1)`\cr
#'   Should the insample performance of the [mlr3::LearnerRegr] be asserted after updating the surrogate?
#'   If the assertion fails (i.e., the insample performance based on the `perf_measure` does not meet the
#'   `perf_threshold`), an error is thrown.
#'   Default is `FALSE`.
#' }
#' \item{`perf_measure`}{List of [mlr3::MeasureRegr]\cr
#'   Performance measures which should be use to assert the insample performance of the [mlr3::LearnerRegr].
#'   Only relevant if `assert_insample_perf = TRUE`.
#'   Default is [mlr3::mlr_measures_regr.rsq] for each learner.
#' }
#' \item{`perf_threshold`}{List of `numeric(1)`\cr
#'   Thresholds the insample performance of the [mlr3::LearnerRegr] should be asserted against.
#'   Only relevant if `assert_insample_perf = TRUE`.
#'   Default is `0` for each learner.
#' }
#' \item{`catch_errors`}{`logical(1)`\cr
#'   Should errors during updating the surrogate be caught and propagated to the `loop_function` which can then handle
#'   the failed infill optimization (as a result of the failed surrogate) appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Default is `TRUE`.
#' }
#' }
#'
#' @export
SurrogateLearnerCollection = R6Class("SurrogateLearnerCollection",
  inherit = Surrogate,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (list of [mlr3::LearnerRegr]).
    #' @param archive (`NULL` | [bbotk::Archive]).
    #' @param x_cols (`NULL` | `character()`).
    #' @param y_cols (`NULL` | `character()`).
    initialize = function(learners, archive = NULL, x_cols = NULL, y_cols = NULL) {
      addresses = map(learners, address)
      if (length(unique(addresses)) != length(addresses)) {
        stop("Redundant Learners must be unique in memory, i.e., deep clones.")
      }
      assert_learners(learners)
      for (learner in learners) {
        if (learner$predict_type != "se" && "se" %in% learner$predict_types) {
          learner$predict_type = "se"
        }
      }

      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_character(x_cols, min.len = 1L, null.ok = TRUE)
      assert_character(y_cols, len = length(learners), null.ok = TRUE)

      ps = ParamSet$new(list(
        ParamLgl$new("assert_insample_perf"),
        ParamUty$new("perf_measures", custom_check = function(x) check_list(x, types = "MeasureRegr", any.missing = FALSE, len = length(learners))),  # FIXME: actually want check_measures
        ParamUty$new("perf_thresholds", custom_check = function(x) check_double(x, lower = -Inf, upper = Inf, any.missing = FALSE, len = length(learners))),
        ParamLgl$new("catch_errors"))
      )
      ps$values = list(assert_insample_perf = FALSE, catch_errors = TRUE)
      ps$add_dep("perf_measures", on = "assert_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_thresholds", on = "assert_insample_perf", cond = CondEqual$new(TRUE))

      super$initialize(model = learners, archive = archive, x_cols = x_cols, y_cols = y_cols, param_set = ps)
    },

    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one `y_col`.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #' New data.
    #'
    #' @return list of [data.table::data.table()]s with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)
      xdt = fix_xdt_missing(xdt, x_cols = self$x_cols, archive = self$archive)
      xdt = char_to_fct(xdt)

      preds = lapply(self$model, function(model) {
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

    #' @field print_id (`character`)\cr
    #' Id used when printing.
    print_id = function(rhs) {
      if (missing(rhs)) {
        paste0("(", paste0(map_chr(self$model, function(model) class(model)[1L]), collapse = " | "), ")")
      } else {
        stop("'print_id' field is read-only.")
      }
    },

    #' @field n_learner (`integer(1)`)\cr
    #' Returns the number of [mlr3::Learner]s.
    n_learner = function() {
      length(self$model)
    },

    #' @field assert_insample_perf (`numeric()`)\cr
    #' Asserts whether the current insample performance meets the performance threshold.
    assert_insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stop("assert_insample_perf is read-only.")
      }

      if (!self$param_set$values$assert_insample_perf) {
        invisible(self$insample_perf)
      }

      check = all(pmap_lgl(
        list(
          insample_perf = self$insample_perf,
          perf_threshold = self$param_set$values$perf_thresholds %??% rep(0, self$n_learner),
          perf_measure = self$param_set$values$perf_measures %??% replicate(length(learners), mlr_measures$get("regr.rsq"), simplify = FALSE)
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
        stop("Current insample performance of the Surrogate Model does not meet the performance threshold.")
      }
      invisible(self$insample_perf)
    },

    #' @field packages (`character()`)\cr
    #' Set of required packages.
    packages = function(rhs) {
      if (missing(rhs)) {
        unique(unlist(map(self$model, "packages")))
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @field feature_types (`character()`)\cr
    #' Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
    #' A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
    feature_types = function(rhs) {
      if (missing(rhs)) {
        Reduce(intersect, map(self$model, "feature_types"))
      } else {
        stop("'feature_types' field is read-only.")
      }
    },

    #' @field properties (`character()`)\cr
    #' Stores a set of properties/capabilities the learner has.
    #' A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
    properties = function(rhs) {
      if (missing(rhs)) {
        Reduce(intersect, map(self$model, "properties"))
      } else {
        stop("'properties' field is read-only.")
      }
    }

  ),

  private = list(

    # Train model with new data.
    # Also calculates the insample performance based on the `perf_measures` hyperparameter if `assert_insample_perf = TRUE`.
    .update = function() {
      xydt = char_to_fct(self$archive$data[, c(self$x_cols, self$y_cols), with = FALSE])
      backend = as_data_backend(xydt)  # we do this here to save time in the lapply below
      features = setdiff(names(xydt), self$y_cols)

      tasks = lapply(self$y_cols, function(y_col) {
        # If this turns out to be a bottleneck, we can also operate on a single task here.
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
      names(self$model) = self$y_cols

      if (self$param_set$values$assert_insample_perf) {
        private$.insample_perf = setNames(pmap_dbl(list(model = self$model, task = tasks, perf_measure = self$param_set$values$perf_measures %??% replicate(length(learners), mlr_measures$get("regr.rsq"), simplify = FALSE)),
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
