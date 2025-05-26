#' @title Dictionary of Input Transformations
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [InputTrafo].
#' Each input transformation has an associated help page, see `mlr_input_trafos[id]`.
#'
#' For a more convenient way to retrieve and construct an input trafo, see [it()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Input Transformation
#' @seealso
#' Sugar function: [it()]
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mlr_input_trafos)
#' it("unitcube")
mlr_input_trafos = R6Class("DictionaryInputTrafo", inherit = Dictionary, cloneable = FALSE)$new()

#' @export
as.data.table.DictionaryInputTrafo= function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    ot = withCallingHandlers(x$get(key), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = ot$label, man = ot$man),
      if (objects) list(object = list(ot))
    )
  }, .fill = TRUE), "key")[]
}

