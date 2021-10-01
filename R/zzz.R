#' @importFrom R6 R6Class
#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import bbotk
#' @import lgr
#' @import mlr3
#' @import mlr3tuning
#' @importFrom stats runif
#' @useDynLib mlr3mbo c_sms_indicator c_eps_indicator
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)

  # add mbo to optimizer dictionary
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  x$add("mbo", OptimizerMbo)

  # setup logger
  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
