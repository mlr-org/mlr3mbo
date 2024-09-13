#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_tuners_adbo
#'
#' @description
#' Asynchronous Decentralized Bayesian Optimization (ADBO).
#'
#' @note
#' The \eqn{\lambda} parameter of the upper confidence bound acquisition function controls the trade-off between exploration and exploitation.
#' A large \eqn{\lambda} value leads to more exploration, while a small \eqn{\lambda} value leads to more exploitation.
#' ADBO can use periodic exponential decay to reduce \eqn{\lambda} periodically to the exploitation phase.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`\cr
#'   \eqn{\lambda} value used for the confidence bound.
#'   Defaults to `1.96`.}
#' \item{`exponential_decay`}{`lgl(1)`\cr
#'   Whether to use periodic exponential decay for \eqn{\lambda}.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`t`}{`integer(1)`\cr
#'   Period of the exponential decay.}
#' \item{`initial_design_size`}{`integer(1)`\cr
#'   Size of the initial design.}
#' \item{`initial_design`}{`data.table`\cr
#'   Initial design.}
#' }
#'
#' @export
TunerADBO = R6Class("TunerADBO",
  inherit = mlr3tuning::TunerAsyncFromOptimizerAsync,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     super$initialize(
       optimizer = OptimizerADBO$new(),
       man = "mlr3tuning::mlr_tuners_adbo"
     )
   }
  )
)

mlr_tuners$add("adbo", TunerADBO)

#' @include aaa.R
tuners[["adbo"]] = TunerADBO
