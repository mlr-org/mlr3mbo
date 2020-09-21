#' @title Surrogate Model for Single criteria response surfaces
#'
#' @description
#' Surrogate Model for Single criteria response surfaces
#'
#' @export
SurrogateSingleCrit = R6Class("SurrogateSingleCrit",
  inherit = Surrogate,

  public = list(
    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one dimension.
    #'
    #' @param xdt [data.table::data.table]\cr
    #' New data.
    #'
    #' @return [data.table::data.table] with the columns `mean` and `se`.
    predict = function(xdt) {
      stop("abstract")
    }
  )

)

