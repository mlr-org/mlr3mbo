#' @title Asynchronous Model Based Optimization
#'
#' @name mlr_tuners_async_mbo
#'
#' @description
#' `TunerAsyncMbo` class that implements Asynchronous Model Based Optimization (MBO).
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
