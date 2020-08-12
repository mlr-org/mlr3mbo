#' @title Surrogate Model for MultiCriteria response surfaces
#'
#' @description
#'
#' @export
SurrogateMultiCrit = R6Class("SurrogateMultiCrit",
  inherit = Surrogate

  public = list(
    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one dimension.
    #'
    #' @return [list] of [data.table::data.table] objects.
    #'   Each contains the columns `mean` and `se`.
    predict = function(xdt) {
      stop("abstract")
    }
  )
    
)

