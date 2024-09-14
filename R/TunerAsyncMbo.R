#' @title Asynchronous Model Based Tuning
#'
#' @include OptimizerAsyncMbo.R
#' @name mlr_tuners_async_mbo
#'
#' @description
#' `TunerAsyncMbo` class that implements asynchronous Model Based Tuning (MBO).
#'
#' @section Parameters:
#' \describe{
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
#' @template param_surrogate
#' @template param_acq_function
#' @template param_acq_optimizer
#'
#' @param param_set [paradox::ParamSet]\cr
#'  Set of control parameters.
#'
#' @export
TunerAsyncMbo = R6Class("TunerAsyncMbo",
  inherit = mlr3tuning::TunerAsyncFromOptimizerAsync,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function(surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, param_set = NULL) {
    optimizer = OptimizerAsyncMbo$new(
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = acq_optimizer,
      param_set = param_set
    )

     super$initialize(
       optimizer = optimizer,
       man = "mlr3tuning::mlr_tuners_async_mbo"
     )
   }
  )
)

mlr_tuners$add("async_mbo", TunerAsyncMbo)

#' @include aaa.R
tuners[["async_mbo"]] = TunerAsyncMbo
