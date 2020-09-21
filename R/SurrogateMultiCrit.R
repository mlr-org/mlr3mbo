#' @title Surrogate Model for MultiCriteria response surfaces
#'
#' @description
#' Surrogate Model for MultiCriteria response surfaces
#'
#' @export
SurrogateMultiCrit = R6Class("SurrogateMultiCrit",
  inherit = Surrogate,

  public = list(
    #' @description
    #' Returns a named list of data.tables.
    #' Each contains the mean response and standard error for one dimension.
    #'
    #' @param xdt [data.table::data.table]\cr
    #' New data.
    #'
    #' @return [list] of [data.table::data.table] objects.
    #'   Each contains the columns `mean` and `se`.
    predict = function(xdt) {
      stop("abstract")
    }
  ),

  active = list(

    #' @field k
    #' Returns number of models.
    k = function() stop("abstract") #FIXME: Should we allow SurrogateMultiCrit for arbitrary d?
  )

)

