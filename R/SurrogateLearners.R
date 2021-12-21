#' @title Surrogate Model Containing Multiple Learners
#'
#' @description
#' Surrogate model containing multiple regression [mlr3::Learner]s.
#' The [mlr3::Learner]s are fit on the target variables as indicated via `y_cols`.
#' Note that redundant [mlr3::Learner]s must be deep clones.
#'
#' @export
SurrogateLearners = R6Class("SurrogateLearners",
  inherit = Surrogate,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (list of [mlr3::LearnerRegr]).
    #' @param archive (`NULL` | [bbotk::Archive]).
    #' @param y_cols (`NULL` | `character()`).
    initialize = function(learners, archive = NULL, y_cols = NULL) {
      # FIXME: deep clone learners?
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

      assert_character(y_cols, len = length(learners), null.ok = TRUE)

      ps = ParamSet$new(list(
        ParamLgl$new("calc_insample_perf"),
        ParamUty$new("perf_measures", custom_check = function(x) check_list(x, types = "MeasureRegr", any.missing = FALSE, len = length(learners))),  # FIXME: actually want check_measures
        ParamUty$new("perf_thresholds", custom_check = function(x) check_double(x, lower = -Inf, upper = Inf, any.missing = FALSE, len = length(learners))))
      )
      ps$values = list(calc_insample_perf = FALSE, perf_measures = replicate(length(learners), msr("regr.rsq"), simplify = FALSE), perf_thresholds = rep(0, length(learners)))  # NOTE: deep clone?
      ps$add_dep("perf_measures", on = "calc_insample_perf", cond = CondEqual$new(TRUE))
      ps$add_dep("perf_thresholds", on = "calc_insample_perf", cond = CondEqual$new(TRUE))

      super$initialize(model = learners, archive = archive, y_cols = y_cols, param_set = ps)
    },

    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one `y_col`.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data.
    #'
    #' @return list of [data.table::data.table()]s with the columns `mean` and `se`.
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

      preds = lapply(self$model, function(model) {
        pred = model$predict_newdata(newdata = char_to_fct(xdt))
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

    #' @field n_learner (`integer(1)`)\cr
    #'   Returns the number of [mlr3::Learner]s.
    n_learner = function() {
      length(self$model)
    },

    #' @field assert_insample_perf (`numeric()`)\cr
    #'   Asserts whether the current insample performance meets the performance threshold.
    assert_insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stop("assert_insample_perf is read-only.")
      }

      if (!self$param_set$values$calc_insample_perf) {
        invisible(self$insample_perf)
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
        stop("Current insample performance of the Surrogate Model does not meet the performance threshold.")
      }
      invisible(self$insample_perf)
    }
  ),

  private = list(

    # Train model with new data.
    # Also calculates the insample performance based on the `perf_measures` hyperparameter if `calc_insample_perf = TRUE`.
    .update = function() {
      xydt = self$archive$data[, c(self$archive$cols_x, self$y_cols), with = FALSE]
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
