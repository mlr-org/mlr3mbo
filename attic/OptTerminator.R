# Optimization Terminator
OptTerminator = R6Class(
  "OptTerminator",

  public = list(
    # public member
    storage = list(),

    # constructor
    initialize = function() {
    },

    # public methods
    # returns true if we can continue our optimization
    can_continue = function(opt_state) {
      stop("Not implemented!")
    },

    # hook called at begin of step
    step_begin = function(opt_state) {

    },

    # hook called at end of step
    step_end = function(opt_state) {

    }


  )
)

