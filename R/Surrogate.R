#' @title Surrogate Model
#'
#' @description
#' Surrogate Model
#'
#' @export
Surrogate = R6Class("Surrogate",
  public = list(

    #' @field model \cr
    #' Stores surrogate model
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
    #' @param xydt [data.table::data.table].
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
    #' @return `NULL`
    update = function(xydt, y_cols) {
      stop("Abstract")
    },

    #' @description
    #' Possible setup routine of the surrogate
    #'
    #' @param xydt [data.table::data.table]\cr
    #' New data.
    #'
    #' @param y_cols (`character()`)\cr
    #' Names of response columns.
    #'
    #' @return `NULL`
    setup = function(xydt, y_cols) {
      self$update(xydt, y_cols)
    },


    #' @description
    #' Returns mean response and standard error
    #'
    #' @param xdt [data.table::data.table]\cr
    #' New data.
    #'
    #' @return [data.table::data.table] with the columns `mean` and `se`.
    predict = function(xdt) {
      stop("Abstract")
    }
  )
)

