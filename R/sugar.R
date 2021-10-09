#' @title Syntactic Sugar Acquisition Function Construction
#'
#' @description
#' This function complements [mlr_acqfunctions] with functions in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#'
#' @param .key (`character(1)`)\cr
#' Key passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve
#' the object.
#' @param .keys (`character()`)\cr
#' Keys passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve
#' multiple objects.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet], or to be set as public field. See
#' [mlr3misc::dictionary_sugar_get()] for more details.
#'
#' @return
#' * [AcqFunction] for `acqf()`.
#' * list of [AcqFunction]s for `acqfs()`.
#'
#' @export
#' @examples
#' acqf("ei")
#' @export
acqf = function(.key, ...) {
  dictionary_sugar(mlr_acqfunctions, .key, ...)
}

#' @rdname acqf
#' @export
acqfs = function(.keys, ...) {
  dictionary_sugar_mget(mlr_acqfunctions, .keys, ...)
}
