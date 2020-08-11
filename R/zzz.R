#' @importFrom R6 R6Class is.R6
#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import bbotk
#' @import lgr
#' @import mlr3
#' @importFrom utils data head tail
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # setup logger
  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

