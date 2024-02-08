#' @export
TunerADBO = R6Class("TunerADBO",
  inherit = mlr3tuning::TunerFromOptimizer,
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
