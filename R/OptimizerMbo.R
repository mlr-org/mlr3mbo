OptimizerMbo = R6Class("OptimizerMbo",
  inherit = bbotk::Optimizer,

  public = list(

    loop_function = NULL, #FIXME: At this point it DOES look ok to have the different mbo algos as objects, right?
    acq_function = NULL,
    acq_optimizer = NULL, 
    args = NULL,

    initialize = function(loop_function, acq_function, acq_optimizer, args = NULL) {
      param_set = ParamSet$new()
      param_classes = c("ParamDbl") # FIXME: Should depend on the surrogate?
      properties = c("single-crit") # FIXME: Should depend on the settings?
      packages = character() # Maybe not so important? Surrogate package etc?
      super$initialize(param_set, param_classes, properties, packages)
      self$loop_function = assert_function(loop_function)
      self$acq_function = assert_r6(acq_function, "AcqFunction")
      self$acq_optimizer = assert_r6(acq_optimizer, "AcqOptimizer")
      self$args = assert_list(args, names = "named", null.ok = TRUE)
    }
  ),

  private = list(
    .optimize = function(inst) {
      do.call(self$loop_function, c(list(instance = inst, acq_function = self$acq_function, acq_optimizer = self$acq_optimizer), self$args))
    },

    .assign_result = function(inst) {
      #FIXME: Where do we define how to assign the result? i.e. if the problem is stochastic we dont want to chose the best point but the point with the best mean prediction
      super$.assign_result(inst)
    }
  )
)
