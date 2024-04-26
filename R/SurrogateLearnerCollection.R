#' @title Surrogate Model Containing Multiple Learners
#'
#' @description
#' Surrogate model containing multiple [mlr3::LearnerRegr].
#' The [mlr3::LearnerRegr] are fit on the target variables as indicated via `cols_y`.
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
#'   the failed acquisition function optimization (as a result of the failed surrogate) appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Default is `TRUE`.
#' }
#' }
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud") &
#'     requireNamespace("ranger")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchMultiCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'   xdt = generate_design_random(instance$search_space, n = 4)$data
#'
#'   instance$eval_batch(xdt)
#'
#'   learner1 = default_gp()
#'
#'   learner2 = default_rf()
#'
#'   surrogate = srlrn(list(learner1, learner2), archive = instance$archive)
#'
#'   surrogate$update()
#'
#'   surrogate$learner
#'
#'   surrogate$learner[["y2"]]$model
#' }
SurrogateLearnerCollection = R6Class("SurrogateLearnerCollection",
  inherit = Surrogate,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (list of [mlr3::LearnerRegr]).
    #' @template param_archive_surrogate
    #' @template param_cols_x_surrogate
    #' @template param_cols_y_surrogate
    initialize = function(learners, archive = NULL, cols_x = NULL, cols_y = NULL) {
      assert_learners(learners)
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

      assert_character(cols_x, min.len = 1L, null.ok = TRUE)
      assert_character(cols_y, len = length(learners), null.ok = TRUE)

      ps = ParamSet$new(list(
        ParamLgl$new("assert_insample_perf"),
        ParamUty$new("perf_measures", custom_check = function(x) check_list(x, types = "MeasureRegr", any.missing = FALSE, len = length(learners))),  # FIXME: actually want check_measures
        ParamUty$new("perf_thresholds", custom_check = function(x) check_double(x, lower = -Inf, upper = Inf, any.missing = FALSE, len = length(learners))),
        ParamLgl$new("catch_errors"))
      )
      ps$values = list(assert_insample_perf = FALSE, catch_errors = TRUE)
      ps$add_dep("perf_measures", on = "assert_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_thresholds", on = "assert_insample_perf", cond = CondEqual$new(TRUE))

      super$initialize(learner = learners, archive = archive, cols_x = cols_x, cols_y = cols_y, param_set = ps)
    },

    #' @description
    #' Predict mean response and standard error.
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one `col_y`.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
    #'
    #' @return list of [data.table::data.table()]s with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)
      xdt = fix_xdt_missing(xdt, cols_x = self$cols_x, archive = self$archive)

      preds = lapply(self$learner, function(learner) {
        pred = learner$predict_newdata(newdata = xdt)
        if (learner$predict_type == "se") {
          data.table(mean = pred$response, se = pred$se)
        } else {
          data.table(mean = pred$response)
        }
      })
      names(preds) = names(self$learner)
      preds
    }
  ),

  active = list(

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        paste0("(", paste0(map_chr(self$learner, function(learner) class(learner)[1L]), collapse = " | "), ")")
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @template field_n_learner_surrogate
    n_learner = function() {
      length(self$learner)
    },

    #' @template field_assert_insample_perf_surrogate
    assert_insample_perf = function(rhs) {
      if (missing(rhs)) {
        check = all(pmap_lgl(
          list(
            insample_perf = self$insample_perf,
            perf_threshold = self$param_set$values$perf_thresholds %??% rep(0, self$n_learner),
            perf_measure = self$param_set$values$perf_measures %??% replicate(self$n_learner, mlr_measures$get("regr.rsq"), simplify = FALSE)
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
      } else {
        stop("$assert_insample_perf is read-only.")
      }
    },

    #' @template field_packages_surrogate
    packages = function(rhs) {
      if (missing(rhs)) {
        unique(unlist(map(self$learner, "packages")))
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        Reduce(intersect, map(self$learner, "feature_types"))
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @template field_properties_surrogate
    properties = function(rhs) {
      if (missing(rhs)) {
        Reduce(intersect, map(self$learner, "properties"))
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_predict_type_surrogate
    predict_type = function(rhs) {
      if (missing(rhs)) {
        predict_types = Reduce(intersect, map(self$learner, "predict_type"))
        if (length(predict_types) == 0L) {
          stop("Learners have different active predict types.")
        }
        predict_types
      } else {
        stop("$predict_type is read-only. To change it, modify $predict_type of the learner directly.")
      }
    }
  ),

  private = list(

    # Train learner with new data.
    # Also calculates the insample performance based on the `perf_measures` hyperparameter if `assert_insample_perf = TRUE`.
    .update = function() {
      xydt = self$archive$data[, c(self$cols_x, self$cols_y), with = FALSE]
      features = setdiff(names(xydt), self$cols_y)
      tasks = lapply(self$cols_y, function(col_y) {
        # if this turns out to be a bottleneck, we can also operate on a single task here
        task = TaskRegr$new(id = paste0("surrogate_task_", col_y), backend = xydt[, c(features, col_y), with = FALSE], target = col_y)
        task
      })
      pmap(list(learner = self$learner, task = tasks), .f = function(learner, task) {
        assert_learnable(task, learner = learner)
        learner$train(task)
        invisible(NULL)
      })
      names(self$learner) = self$cols_y

      if (self$param_set$values$assert_insample_perf) {
        private$.insample_perf = setNames(pmap_dbl(list(learner = self$learner, task = tasks, perf_measure = self$param_set$values$perf_measures %??% replicate(self$n_learner, mlr_measures$get("regr.rsq"), simplify = FALSE)),
          .f = function(learner, task, perf_measure) {
            assert_measure(perf_measure, task = task, learner = learner)
            learner$predict(task)$score(perf_measure, task = task, learner = learner)
          }
        ), nm = map_chr(self$param_set$values$perf_measures, "id"))
        self$assert_insample_perf
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        learner = map(value, function(x) x$clone(deep = TRUE)),
        .param_set = value$clone(deep = TRUE),
        .archive = value$clone(deep = TRUE),
        value
      )
    }
  )
)

