#' @title Dictionary of Output Transformations
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [OutputTrafo].
#' Each output transformation has an associated help page, see `mlr_output_trafos[id]`.
#'
#' For a more convenient way to retrieve and construct an output trafo, see [ot()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Output Transformation
#' @seealso
#' Sugar function: [ot()]
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mlr_output_trafos)
#' ot("standardize")
mlr_output_trafos = R6Class("DictionaryOutputTrafo", inherit = Dictionary, cloneable = FALSE)$new()

#' @export
as.data.table.DictionaryOutputTrafo= function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    ot = withCallingHandlers(x$get(key), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = ot$label, man = ot$man),
      if (objects) list(object = list(ot))
    )
  }, .fill = TRUE), "key")[]
}

