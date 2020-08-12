#FIXME: Not the final API!
AcqOptimizer = R6Class("AcqOptimizer",
  public = list(
    initialize = function() {
    },

    optimize = function(acqf) {
      stop("abstract")
    }
  )
)
