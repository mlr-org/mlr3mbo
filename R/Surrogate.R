#' @title Surrogate Model from mlr3 Learner
#'
#' @description
#'
#' @export
Surrogate = R6Class("Surrogate",
  public = list(

    #' @field Stores surrogate model
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
    #' @return `NULL`
    update = function(xydt, y_cols) {
      stop("Abstract")
    },

    #' @description
    #' Possible setup routine of the surrogate
    #'
    #' @return `NULL`
    setup = function(xydt, y_cols) {
      self$update(xydt, y_cols)
    },


    #' @description
    #' Returns mean response and standard error
    #'
    #' @return [data.table::data.table]
    predict = function(xdt) {
      stop("Abstract")
    }
  )
)

