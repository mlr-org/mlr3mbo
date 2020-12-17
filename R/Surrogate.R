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
    #' @return `NULL`
    update = function(xydt, y_cols) {
      stop("Abstract")
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
    #' @return `NULL`
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

    #' @field insample_performance (`numeric()`)\cr
    #' Surrogate Model's current insample performance.
    insample_performance = function(rhs) {
      if (!missing(rhs)) {
        stopf("Field/Binding is read-only")
      }
      private$.insample_performance %??% NaN
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

    #' @field assert_insample_performance (`logical(1)`)\cr
    #' Whether the current insample performance meets the `perf_threshold`.
    assert_insample_performance = function(rhs) {  # FIXME: better name
      stop("Abstract")
    }
  ),

  private = list(

    .insample_performance = NULL,
    .param_set = NULL,

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
