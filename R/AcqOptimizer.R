AcqOptimizer = R6Class("AcqOptimizer", 
  public = list(
    initialize = function() {
    },

    optimize = function(acqf, n_evals) {
      assert_r6(acqf, "AcqFunction")
      n_evals = assert_int(n_evals)
      d = generate_design_random(acqf$domain, n_evals)
      y = acqf$eval_batch(d$data)
      # FIXME:
      which_best = which_min
      j = which_best(y)
      d$data[j,]
    }
  )
)

