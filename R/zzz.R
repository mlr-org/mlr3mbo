#' @importFrom R6 R6Class is.R6
#' @import R6 
#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import lgr
#' @importFrom utils data head tail
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

