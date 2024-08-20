#' @title Dictionary of Acquisition Functions
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [AcqFunction].
#' Each acquisition function has an associated help page, see `mlr_acqfunctions_[id]`.
#'
#' For a more convenient way to retrieve and construct an acquisition function, see [acqf()] and [acqfs()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Acquisition Function
#' @seealso
#' Sugar functions: [acqf()], [acqfs()]
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
    # NOTE: special handling of AcqFunctionMulti due to acq_functions being required as an argument during construction
    if (key == "multi") {
      acq_function1 = AcqFunctionMean$new()
      acq_function2 = AcqFunctionSD$new()
      acqf = withCallingHandlers(x$get(key, acq_functions = list(acq_function1, acq_function2)), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
      insert_named(
        list(key = key, label = "Acquisition Function Wrapping Multiple Acquisition Functions", man = acqf$man),
        if (objects) list(object = list(acqf))
      )
    } else {
      acqf = withCallingHandlers(x$get(key), packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
      insert_named(
        list(key = key, label = acqf$label, man = acqf$man),
        if (objects) list(object = list(acqf))
      )
    }
  }, .fill = TRUE), "key")[]
}

