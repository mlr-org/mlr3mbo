#' @title Dictionary of Result Assigners
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [ResultAssigner].
#' Each acquisition function has an associated help page, see `mlr_result_assigners_[id]`.
#'
#' For a more convenient way to retrieve and construct an acquisition function, see [ras()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Result Assigner
#' @seealso
#' Sugar function: [ras()]
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mlr_result_assigners)
#' ras("archive")
mlr_result_assigners = R6Class("DictionaryResultAssigner", inherit = Dictionary, cloneable = FALSE)$new()

#' @export
as.data.table.DictionaryResultAssigner = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    ras = withCallingHandlers(x$get(key), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = ras$label, man = ras$man),
      if (objects) list(object = list(ras))
    )
  }, .fill = TRUE), "key")[]
}

