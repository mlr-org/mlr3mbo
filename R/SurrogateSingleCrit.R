#' @title Surrogate Model for Single criteria response surfaces
#'
#' @description
#'
#' @export
SurrogateSingleCrit = R6Class("SurrogateSingleCrit",
  inherit = Surrogate,

  public = list(
    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one dimension.
    #'
    #' @return [data.table::data.table] with the columns `mean` and `se`.
    predict = function(xdt) {
      stop("abstract")
    }
  )
    
)

