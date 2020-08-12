#' @export
AcqOptimizerRandomSearch = R6Class("AcqOptimizerRandomSearch",
  inherit = AcqOptimizer,

  public = list(

    param_set = NULL,

    initialize = function() {
      self$param_set = ParamSet()
    },

    optimize = function(acqf) {
      xdt = generate_design_random(acqfun$search_space, 1000)$data
      ydt = acqfun$eval_dt(xdt)
      xydt = cbind(xdt, ydt)
      setorderv(xydt, acqf$codomain$ids(), order = 1, na.last = TRUE)
      xydt[1, acqf$search_space$ids(), with = FALSE]
    }
))
