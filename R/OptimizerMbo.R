OptimizerMbo = R6Class("OptimizerMbo",
  inherit = bbotk::Optimizer,

  public = list(
    proposal_generator = NULL,

    initialize = function(proposal_generator) {
      param_set = ParamSet$new()
      param_classes = c("ParamDbl") # FIXME: Should depend on the surrogate?
      properties = c("single-crit") # FIXME: Should depend on the settings?
      packages = character() # Maybe not so important? Surrogate package etc?
      super$initalize(param_set, param_classes, properties, packages)
      self$proposal_generator = assert_r6(proposal_generator, "ProposalGenerator")
    }
  ),

  private = list(
    .optimize = function(inst) {
      pg = self$proposal_generator
      pg$setup(inst$archive)
      repeat { # iterate until we have an exception from eval_batch
        xdt = pg$propose()
        inst$eval_batch(xdt)
        pg$update()
      }
    },

    .assign_result = function(inst) {
      #FIXME: Where do we define how to assign the result? i.e. if the problem is stochastic we dont want to chose the best point but the point with the best mean prediction
      super$.assign_result(inst)
    }
  )
)
