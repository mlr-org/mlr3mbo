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
      private$.x_cols = x_cols  # assertion is done in SurrogateLearner or SurrogateLearners
      private$.y_cols = y_cols  # assertion is done in SurrogateLearner or SurrogateLearners
      private$.param_set = assert_r6(param_set, classes = "ParamSet")
    },

    #' @description
    #' Train model with new data.
    #'
    #' @return `NULL`.
    update = function() {
      if (is.null(self$archive)) stop("archive must be set during construction or manually prior before calling $update().")
      tryCatch(private$.update(),
        error = function(error_condition) {
          lg$info(error_condition$message)
          stop(set_class(list(message = error_condition$message, call = NULL),
            classes = c("mbo_error", "surrogate_update_error", "error", "condition")))
        }
      )
      invisible(NULL)
    },

    #' @description
    #' Predict mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data.
    #'
    #' @return Arbitrary prediction object.
    predict = function(xdt) {
      stop("Abstract")
    }
  ),

  active = list(

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
    #'   Returns the number of [mlr3::Learner]s.
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
        stopf("insample_perf is read-only.")
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
