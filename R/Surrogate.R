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
    #' @template param_cols_x_surrogate
    #' @template param_cols_y_surrogate
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Parameter space description depending on the subclass.
    initialize = function(learner, archive, cols_x, cols_y, param_set) {
      # most assertions are done in subclasses
      self$learner = learner
      private$.archive = assert_r6(archive, classes = "Archive", null.ok = TRUE)
      private$.cols_x = assert_character(cols_x, min.len = 1L, null.ok = TRUE)
      private$.cols_y = cols_y = assert_character(cols_y, min.len = 1L, null.ok = TRUE)
      assert_r6(param_set, classes = "ParamSet")
      stopifnot(param_set$class[["catch_errors"]] == "ParamLgl")
      private$.param_set = param_set
    },

    #' @description
    #' Train learner with new data.
    #' Subclasses must implement `private.update()` and `private.update_async()`.
    #'
    #' @return `NULL`.
    update = function() {
      if (is.null(self$archive)) stop("Archive must be set during construction or manually prior before calling $update().")
      if (self$param_set$values$catch_errors) {
        if (self$archive_is_async) {
          tryCatch(private$.update_async(),
            error = function(error_condition) {
              lg$warn(error_condition$message)
              stop(set_class(list(message = error_condition$message, call = NULL),
                classes = c("surrogate_update_error", "mbo_error", "error", "condition")))
            }
          )
        } else {
          tryCatch(private$.update(),
            error = function(error_condition) {
              lg$warn(error_condition$message)
              stop(set_class(list(message = error_condition$message, call = NULL),
                classes = c("surrogate_update_error", "mbo_error", "error", "condition")))
            }
          )
        }
      } else {
        if (self$archive_is_async) {
          private$.update_async()
        } else {
          private$.update()
        }
      }
      invisible(NULL)
    },

    #' @description
    #' Reset the surrogate model.
    #' Subclasses must implement `private$.reset()`.
    #'
    #' @return `NULL`
    reset = function() {
      private$.reset()
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
    #'
    #' @return (`character(1)`).
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
        private$.archive = assert_archive(rhs, null_ok = TRUE)
        invisible(private$.archive)
      }
    },

    #' @template field_archive_surrogate_is_async
    archive_is_async = function(rhs) {
      if (missing(rhs)) {
        inherits(private$.archive, "ArchiveAsync")
      } else {
        stop("$archive_is_async is read-only.")
      }
    },

    #' @template field_n_learner_surrogate
    n_learner = function() {
      stop("Abstract.")
    },

    #' @template field_cols_x_surrogate
    cols_x = function(rhs) {
      if (missing(rhs)) {
        if (is.null(private$.cols_x)) self$archive$cols_x else private$.cols_x
      } else {
        private$.cols_x = assert_character(rhs, min.len = 1L)
      }
    },

    #' @template field_cols_y_surrogate
    cols_y = function(rhs) {
      if (missing(rhs)) {
        if (is.null(private$.cols_y)) self$archive$cols_y else private$.cols_y
      } else {
        private$.cols_y = assert_character(rhs, len = self$n_learner)
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

    .cols_x = NULL,

    .cols_y = NULL,

    .insample_perf = NULL,

    .param_set = NULL,

    .update = function() {
      stop("Abstract.")
    },

    .update_async = function() {
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

