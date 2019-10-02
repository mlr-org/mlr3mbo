# Optimization Iterator

# The optimization iterator should not store any temporary function
# It is supposed to contain all the logic that is necessary for one iteration step
OptIterator= R6Class(
  "OptIterator",

  public = list(
    # public member
    id = character(1L),
    storage = list(),

    # constructor
    initialize = function() {

    },

    # returns OptState
    step = function(opt_state) {
      stop("Not implemented!")
    }


  )
)
