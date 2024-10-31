#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_tuners_adbo
#'
#' @description
#' `TunerAsyncMboADBO` class that implements Asynchronous Decentralized Bayesian Optimization (ADBO).
#' ADBO is a variant of Asynchronous Model Based Optimization (AMBO) that uses [AcqFunctionStochasticCB] with exponential lambda decay.
#'
#' @note
#' The lambda parameter of the upper confidence bound acquisition function controls the trade-off between exploration and exploitation.
#' A large lambda value leads to more exploration, while a small lambda value leads to more exploitation.
#' The initial lambda value is drawn from an exponential distribution with rate `1 / lambda`.
#' ADBO can use periodic exponential decay to reduce lambda periodically with the formula  `lambda * exp(-rate * (t %% period))`.
#' The surrogate model is always a random forest and die acquisition optimizer is random search with a budget of 10,000 evaluations.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`\cr
#'   Lambda value for sampling from the exponential distribution.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`period`}{`integer(1)`\cr
#'   Period of the exponential decay.}
#' \item{`initial_design_size`}{`integer(1)`\cr
#'   Size of the initial design.
#'   Defaults to `100`.}
#'
#' \item{`initial_design`}{`data.table::data.table()`\cr
#'   Initial design of the optimization.
#'   If `NULL`, a design of size `design_size` is generated with `design_function`.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the initial design.}
#' \item{`design_function`}{`character(1)`\cr
#'   Function to generate the initial design.
#'   One of `c("random", "sobol", "lhs")`.}
#' \item{`n_workers`}{`integer(1)`\cr
#'   Number of parallel workers.
#'   If `NULL`, all rush workers set with [rush::rush_plan()] are used.}
#' }
#'
#'
#' @references
#' * `r format_bib("egele_2023")`
#'
#' @export
TunerAsyncMboADBO = R6Class("TunerAsyncMboADBO",
  inherit = mlr3tuning::TunerAsyncFromOptimizerAsync,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
    optimizer = OptimizerAsyncMboADBO$new()

     super$initialize(
       optimizer = optimizer,
       man = "mlr3tuning::mlr_tuners_adbo"
     )
   }
  )
)

#' @include aaa.R
tuners[["adbo"]] = TunerAsyncMboADBO
