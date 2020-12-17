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
    #' @param archive [bbotk::Archive].
    #'
    #' @return `NULL`
    update = function(archive) {
      stop("Abstract")
    },

    #' @description
    #' Possible setup routine of the surrogate
    #'
    #' @param archive [bbotk::Archive].
    #'
    #' @return `NULL`
    setup = function(archive) {
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
  )
)
