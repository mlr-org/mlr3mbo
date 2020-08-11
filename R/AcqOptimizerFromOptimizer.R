AcqOptimizerFromOptimizer = R6Class("AcqOptimizerFromOptimizer",
  inherit = AcqOptimizer,

  public = list(

    term = NULL

    initialize = function(optimizer, term) {
      private$.optimizer = assert_optimizer(optimizer)
      self$term = assert_r6(term, "Terminator")
    },

    optimize = function(acqf) {
      assert_r6(acqf, "AcqFunction")
      
      inst = OptimInstanceSingleCrit$new(objective = acqf$generate_objective,
        search_space = acqf$search_space, terminator = self$term)

      res = private$.optimizer$optimize(inst)
      res$result_y
    }),

    private = list(
      .optimizer = NULL
    )
)
