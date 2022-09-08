#' @title Dictionary of Loop Functions
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class `loop_function`.
#' Each loop function has an associated help page, see `mlr_loop_functions_[id]`.
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family loop_function
#' @export
#' @examples
#' as.data.table(mlr_loop_functions)
mlr_loop_functions = R6Class("DictionaryLoopFunction", inherit = Dictionary, cloneable = FALSE)$new()

#' @export
as.data.table.DictionaryLoopFunction = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    lpf = get(key, envir = asNamespace("mlr3mbo"))
    insert_named(
      list(key = key, label = attr(lpf, "label"), instance = attr(lpf, "instance"), man = attr(lpf, "man")),
      if (objects) list(object = list(lpf))
    )
  }, .fill = TRUE), "key")[]
}
