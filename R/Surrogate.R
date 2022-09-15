#' @title Surrogate Model
#'
#' @description
#' Surrogate model.
#'
#' @export
Surrogate = R6Class("Surrogate",
  public = list(

    #' @field model Surrogate model.
    model = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param model (Model).
    #' @param archive (`NULL` | [bbotk::Archive]).
    #' @param x_cols (`NULL` | `character()`).
    #' @param y_cols (`NULL` | `character()`).
    #' @param param_set ([paradox::ParamSet]).
    initialize = function(model, archive, x_cols, y_cols, param_set) {
      # most assertions are done in derived classes
      self$model = model
      private$.archive = archive
      private$.x_cols = x_cols  # assertion is done in SurrogateLearner or SurrogateLearnerCollection
      private$.y_cols = y_cols  # assertion is done in SurrogateLearner or SurrogateLearnerCollection
      assert_r6(param_set, classes = "ParamSet")
      assert_r6(param_set$params$catch_errors, classes = "ParamLgl")
      private$.param_set = param_set
    },

    #' @description
    #' Train model with new data.
    #'
    #' @return `NULL`.
    update = function() {
      if (is.null(self$archive)) stop("Archive must be set during construction or manually prior before calling $update().")
      if (self$param_set$values$catch_errors) {
        tryCatch(private$.update(),
          error = function(error_condition) {
            lg$info(error_condition$message)
            stop(set_class(list(message = error_condition$message, call = NULL),
              classes = c("mbo_error", "surrogate_update_error", "error", "condition")))
          }
        )
      } else {
        private$.update()
      }
      invisible(NULL)
    },

    #' @description
    #' Predict mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #' New data.
    #'
    #' @return Arbitrary prediction object.
    predict = function(xdt) {
      stop("Abstract.")
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), paste0(": ", self$print_id))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values)))
    }
  ),

  active = list(

    #' @field print_id (`character`)\cr
    #' Id used when printing.
    print_id = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("'print_id' field is read-only.")
      }
    },

    #' @field archive ([bbotk::Archive]).
    archive = function(rhs) {
      if (missing(rhs)) {
        private$.archive
      } else {
        private$.archive = assert_r6(rhs, classes = "Archive")
        invisible(private$.archive)
      }
    },

    #' @field n_learner (`integer(1)`)\cr
    #' Returns the number of learners.
    n_learner = function() {
      stop("Abstract.")
    },

    #' @field x_cols (`character()`).
    x_cols = function(rhs) {
      if (missing(rhs)) {
        if (is.null(private$.x_cols)) self$archive$cols_x else private$.x_cols
      } else {
        private$.x_cols = assert_character(rhs, min.len = 1L)
      }
    },


    #' @field y_cols (`character()`).
    y_cols = function(rhs) {
      if (missing(rhs)) {
        if (is.null(private$.y_cols)) self$archive$cols_y else private$.y_cols
      } else {
        private$.y_cols = assert_character(rhs, len = self$n_learner)
      }
    },

    #' @field insample_perf (`numeric()`)\cr
    #' Surrogate model's current insample performance.
    insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stop("insample_perf is read-only.")
      }
      private$.insample_perf %??% NaN
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

    #' @field assert_insample_perf (`logical(1)`)\cr
    #' Asserts whether the current insample performance meets the performance threshold.
    assert_insample_perf = function(rhs) {
      stop("Abstract.")
    },

    #' @field packages (`character`)\cr
    #' Set of required packages. A warning is signaled by the constructor if at least one of the packages is not
    #' installed, but loaded (not attached) later on-demand via 'requireNamespace()'.
    packages = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("'packages' field is read-only.")
      }
    },

    #' @field feature_types (`character()`)\cr
    #' Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
    #' A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
    feature_types = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("'feature_types' field is read-only.")
      }
    },

    #' @field properties (`character()`)\cr
    #' Stores a set of properties/capabilities the learner has.
    #' A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
    properties = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("'properties' field is read-only.")
      }
    }
  ),

  private = list(

    .archive = NULL,

    .x_cols = NULL,

    .y_cols = NULL,

    .insample_perf = NULL,

    .param_set = NULL,

    .update = function() {
      stop("Abstract.")
    },

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
