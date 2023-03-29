#' @title Surrogate Model
#'
#' @description
#' Abstract surrogate model class.
#'
#' A surrogate model is used to model the unknown objective function(s) based on all points evaluated so far.
#'
#' @export
Surrogate = R6Class("Surrogate",
  public = list(

    #' @field learner (learner)\cr
    #'   Arbitrary learner object depending on the subclass.
    learner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner (learner)\cr
    #'   Arbitrary learner object depending on the subclass.
    #' @template param_archive_surrogate
    #' @template param_x_cols_surrogate
    #' @template param_y_cols_surrogate
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Parameter space description depending on the subclass.
    initialize = function(learner, archive, x_cols, y_cols, param_set) {
      # most assertions are done in subclasses
      self$learner = learner
      private$.archive = assert_r6(archive, classes = "Archive", null.ok = TRUE)
      private$.x_cols = assert_character(x_cols, min.len = 1L, null.ok = TRUE)
      private$.y_cols = y_cols = assert_character(y_cols, min.len = 1L, null.ok = TRUE)
      assert_r6(param_set, classes = "ParamSet")
      assert_r6(param_set$params$catch_errors, classes = "ParamLgl")
      private$.param_set = param_set
    },

    #' @description
    #' Train learner with new data.
    #' Subclasses must implement `$private.update()`.
    #'
    #' @return `NULL`.
    update = function() {
      if (is.null(self$archive)) stop("Archive must be set during construction or manually prior before calling $update().")
      if (self$param_set$values$catch_errors) {
        tryCatch(private$.update(),
          error = function(error_condition) {
            lg$warn(error_condition$message)
            stop(set_class(list(message = error_condition$message, call = NULL),
              classes = c("surrogate_update_error", "mbo_error", "error", "condition")))
          }
        )
      } else {
        private$.update()
      }
      invisible(NULL)
    },

    #' @description
    #' Predict mean response and standard error.
    #' Must be implemented by subclasses.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
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

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @template field_archive_surrogate
    archive = function(rhs) {
      if (missing(rhs)) {
        private$.archive
      } else {
        private$.archive = assert_r6(rhs, classes = "Archive")
        invisible(private$.archive)
      }
    },

    #' @template field_n_learner_surrogate
    n_learner = function() {
      stop("Abstract.")
    },

    #' @template field_x_cols_surrogate
    x_cols = function(rhs) {
      if (missing(rhs)) {
        if (is.null(private$.x_cols)) self$archive$cols_x else private$.x_cols
      } else {
        private$.x_cols = assert_character(rhs, min.len = 1L)
      }
    },

    #' @template field_y_cols_surrogate
    y_cols = function(rhs) {
      if (missing(rhs)) {
        if (is.null(private$.y_cols)) self$archive$cols_y else private$.y_cols
      } else {
        private$.y_cols = assert_character(rhs, len = self$n_learner)
      }
    },

    #' @field insample_perf (`numeric()`)\cr
    #'   Surrogate model's current insample performance.
    insample_perf = function(rhs) {
      if (missing(rhs)) {
        private$.insample_perf %??% NaN
      } else {
        stop("$insample_perf is read-only.")
      }
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #'   Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$param_set is read-only.")
      } else {
        private$.param_set
      }
    },

    #' @template field_assert_insample_perf_surrogate
    assert_insample_perf = function(rhs) {
      stop("Abstract.")
    },

    #' @template field_packages_surrogate
    packages = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @template field_properties_surrogate
    properties = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_predict_type_surrogate
    predict_type = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("$predict_type is read-only. To change it, modify $predict_type of the learner directly.")
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
        .archive = value$clone(deep = TRUE),
        value
      )
    }
  )
)

