AcqOptimizerFromOptimizer = R6Class("AcqOptimizerFromOptimizer",
  inherit = AcqOptimizer,

  public = list(

    initialize = function(optimizer) {
      private$.optimizer = assert_optimizer(optimizer)

    },

    optimize = function(acqf, n_evals) {
      assert_r6(acqf, "AcqFunction")
      n_evals = assert_int(n_evals)

      inst = OptimInstanceSingleCrit$new(objective = acqf$generate_objective,
        search_space = acqf$search_space, terminator = trm("evals",
        n_evals = n_evals))

      res = private$.optimizer$optimize(inst)
      res$result_y
    }),

    private = list(
      .optimizer = NULL
    )
)
