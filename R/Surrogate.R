#' @title Surrogate Model
#'
#' @description
#' Surrogate model.
#'
#' @export
Surrogate = R6Class("Surrogate",
  public = list(

    #' @field model Surrogate model
    model = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param model (Model).
    initialize = function(model) {
      stop("Abstract")
    },

    #' @description
    #' Train model with new data.
    #'
    #' @param xydt ([data.table::data.table()])\cr
    #'   New data.
    #'
    #' @param y_cols (`character()`)\cr
    #'   Name(s) of response column(s).
    #'
    #' @return `NULL`.
    update = function(xydt, y_cols) {
      tryCatch(private$.update(xydt, y_cols = y_cols),
        error = function(error_condition) {
          lg$info(error_condition$message)  # FIXME: logging?
          stop(set_class(list(message = error_condition$message, call = NULL),
            classes = c("leads_to_exploration_error", "update_error", "error", "condition")))
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

    #' @field insample_perf (`numeric()`)\cr
    #' Surrogate model's current insample performance.
    insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only.")
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
      stop("Abstract")
    }
  ),

  private = list(

    .insample_perf = NULL,
    .param_set = NULL,

    .update = function(xydt, y_cols) {
      stop("Abstract")
    },

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
