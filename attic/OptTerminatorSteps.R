# Optimization Terminator Steps
OptTerminatorSteps = R6Class(
  "OptTerminatorSteps",
  inherit = OptTerminator,

  public = list(

    # constructor
    initialize = function(max_steps = 20L) {
      super$initialize()
      self$storage$max_steps = assert_int(max_steps)
    },

    # public methods
    # returns true if we can continue our optimization
    can_continue = function(opt_state) {
      self$storage$max_steps > opt_state$step_counter
    }
  )
)

