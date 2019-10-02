# Optimization State

# The OptState stores all temporary information for the optimization
# It is not supposed to have any optimization logic

OptState= R6Class(
  "OptState",

  public = list(
    # public member
    storage = list(),
    opt_problem = NULL,
    opt_iterator = NULL,
    opt_terminator = NULL,
    surrogate_model = NULL,
    proposal_generator = NULL,
    hook_storage = list(),
    step_counter = 0,

    # constructor
    initialize = function(opt_problem, opt_iterator, opt_terminator, surrogate_model, proposal_generator) {
      self$opt_problem = opt_problem
      self$opt_iterator = opt_iterator
      self$opt_terminator = opt_terminator
      self$surrogate_model = surrogate_model
      self$proposal_generator = proposal_generator
    },

    # public methods
    # returns true if we can continue our optimization
    can_continue = function() {
      self$opt_terminator$can_continue(self)
    },

    # tell the iterator to do its work for this step
    step = function() {
      self$opt_terminator$step_begin(self)
      self$step_counter = self$step_counter + 1L
      self$opt_iterator$step(self)
      self$opt_terminator$step_end(self)
      self$hook_storage[[self$step_counter]] = self$opt_problem$hook_fun(self)
    },

    # fabricate the optimization result
    terminate = function() {
      return(self) #just for the moment
    }

  )
)
