# Optimization Problem

# The OptProblem is an object that is not allowed to change during optimization
# It contains all information to start the optimization
OptProblem= R6Class(
  "OptProblem",

  public = list(
    # public member
    storage = list(),
    target_fun = NULL,
    design = NULL,
    opt_iterator = NULL,
    opt_terminator = NULL,
    surrogate_model = NULL,
    proposal_generator = NULL,
    hook_fun = NULL,

    # constructor
    initialize = function(target_fun, design, opt_iterator, opt_terminator, surrogate_model, proposal_generator, hook_fun = function(opt_state) NULL) {
      self$target_fun = target_fun
      self$design = design
      self$opt_iterator = opt_iterator
      self$opt_terminator = opt_terminator
      self$surrogate_model = surrogate_model
      self$proposal_generator = proposal_generator
      self$hook_fun = hook_fun
    },

    # public methods
    # returns OptState
    init = function() {
      OptState$new(
        opt_problem = self,
        opt_iterator = self$opt_iterator$clone(deep = TRUE),
        opt_terminator = self$opt_terminator$clone(deep = TRUE),
        surrogate_model = self$surrogate_model$clone(deep = TRUE),
        proposal_generator = self$proposal_generator$clone(deep = TRUE)
      )
    }
  )
)
