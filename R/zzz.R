#' @include aaa.R
#' @importFrom R6 R6Class
#' @import checkmate
#' @import data.table
#' @import paradox
#' @import spacefillr
#' @import mlr3misc
#' @import bbotk
#' @import lgr
#' @import mlr3
#' @import mlr3tuning
#' @importFrom stats setNames runif dnorm pnorm quantile rexp sd
#' @useDynLib mlr3mbo c_sms_indicator c_eps_indicator
"_PACKAGE"

register_bbotk = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  iwalk(optimizers, function(obj, nm) x$add(nm, obj))
} # nocov end

register_mlr3tuning = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  iwalk(tuners, function(obj, nm) x$add(nm, obj))
} # nocov end

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  register_namespace_callback(pkgname, "bbotk", register_bbotk)
  register_namespace_callback(pkgname, "mlr3tuning", register_mlr3tuning)

  #https://github.com/mlr-org/bbotk/blob/ae6cac60f71b3c44ce1bb29669f5d06cddeb95d4/R/zzz.R#L20
  lg = lgr::get_logger("mlr3/bbotk")
  assign("lg", lg, envir = parent.env(environment()))

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

.onUnload = function(libpaths) { # nolint
  # nocov start
  walk(names(optimizers), function(id) bbotk::mlr_optimizers$remove(id))
  walk(names(tuners), function(id) mlr3tuning::mlr_tuners$remove(id))
} # nocov end

# static code checks should not complain about commonly used data.table columns
utils::globalVariables("y_scal")

if (!("mlr3mbo" %in% Sys.getenv("DEVTOOLS_LOAD"))) {
  leanify_package()
}
