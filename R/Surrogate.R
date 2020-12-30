#' @title Surrogate Model
#'
#' @description
#' Surrogate Model
#'
#' @export
Surrogate = R6Class("Surrogate",
  public = list(

    #' @field model Surrogate Model
    model = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param model Model
    initialize = function(model) {
      stop("Abstract")
    },

    #' @description
    #' Train model with new points.
    #'
    #' @param xydt [data.table::data.table()]\cr
    #' Desing of new points.
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
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
    #' Possible setup routine of the surrogate
    #'
    #' @param xydt [data.table::data.table()]\cr
    #' Initial design.
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
    #'
    #' @return `NULL`.
    setup = function(xydt, y_cols) {
      assert_xydt(xydt, y_cols)
      self$update(xydt, y_cols)
    },


    #' @description
    #' Returns mean response and standard error
    #'
    #' @param xdt [data.table::data.table()]\cr
    #' New data.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      stop("Abstract")
    }
  ),

  active = list(

    #' @field insample_perf (`numeric()`)\cr
    #' Surrogate Model's current insample performance.
    insample_perf = function(rhs) {
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only")
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
