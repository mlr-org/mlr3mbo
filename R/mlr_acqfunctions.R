#' @title Dictionary of Acquisition Functions
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [AcqFunction].
#' Each acquisition function has an associated help page, see `mlr_acqfunctions_[id]`.
#'
#' For a more convenient way to retrieve and construct an acquisition function, see [acqf()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Acquisition Function
#' @seealso
#' Sugar function: [acqf()]
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mlr_acqfunctions)
#' acqf("ei")
mlr_acqfunctions = R6Class("DictionaryAcqFunction", inherit = Dictionary, cloneable = FALSE)$new()

#' @export
as.data.table.DictionaryAcqFunction = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    acqf = withCallingHandlers(x$get(key), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = acqf$label, man = acqf$man),
      if (objects) list(object = list(acqf))
    )
  }, .fill = TRUE), "key")[]
}

