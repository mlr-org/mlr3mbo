#' @title Surrogate Model for Single Criteria Response Surfaces
#'
#' @description
#' Surrogate model for single criteria response surfaces.
#'
#' @export
SurrogateSingleCrit = R6Class("SurrogateSingleCrit",
  inherit = Surrogate,

  public = list(
    #' @description
    #' Predict mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      stop("abstract")
    }
  )
)
