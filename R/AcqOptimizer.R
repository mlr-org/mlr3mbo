AcqOptimizer = R6Class("AcqOptimizer", 
  public = list(
    initialize = function() {
    },

    optimize = function(acqf, n_evals) {
      d = generate_design_random(acqf$param_set, n_evals)
      dd = d$data
      y = acqf$eval_batch(dd)
      # FIXME:
      which_best = which_min
      j = which_best(y)
      dd[j,]
    }
  )
)

