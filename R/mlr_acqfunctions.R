#' @title Dictionary of Acquisition Functions
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [AcqFunction].
#' Each acquisition function has an associated help page, see `mlr_acqfunction_[id]`.
#'
#' For a more convenient way to retrieve and construct an acquisition function, see
#' [acqf()]/[acqfs()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Optimizer
#' @seealso
#' Sugar functions: [acqf()], [acqfs()]
#' @export
#' @examples
#' acqf("ei")
mlr_acqfunctions = R6Class("DictionaryAcqFunction", inherit = Dictionary, cloneable = FALSE)$new()

