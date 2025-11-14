#' @title Dictionary of Acquisition Function Optimizers
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [AcqOptimizer].
#' Each input transformation has an associated help page, see `mlr_acqoptimizers[id]`.
#'
#' For a more convenient way to retrieve and construct an acquisition function optimizer, see [acqo()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Acquisition Function Optimizer
#' @seealso
#' Sugar function: [acqo()]
#' @export
#' @examples
#' library(data.table)
#' as.data.table(mlr_acqoptimizers)
#' acqo("local_search")
mlr_acqoptimizers = R6Class("DictionaryAcqOptimizer", inherit = Dictionary, cloneable = FALSE)$new()

#' @export
as.data.table.DictionaryAcqOptimizer= function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    acqopt = withCallingHandlers(x$get(key), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = acqopt$label, man = acqopt$man),
      if (objects) list(object = list(acqopt))
    )
  }, .fill = TRUE), "key")[]
}

