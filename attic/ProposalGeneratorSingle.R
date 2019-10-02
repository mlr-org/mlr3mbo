ProposalGeneratorSingle = R6Class(
  "ProposalGeneratorSingle",
  inherit = ProposalGenerator,
  
  public = list(
    # public member
    acq_function = NULL,
    acq_optimizer = NULL,
    
    # constructor
    initialize = function(acq_function, acq_optimizer) {
      self$acq_function = acq_function
      self$acq_optimizer = acq_optimizer
    },
    
    # public methods
    # value: data.table with column x (plus additional stuff in additional columns?)
    generate = function(opt_state) {
      self$acq_function$prepare(opt_state)
      self$acq_optimizer$optim(self$acq_function)  
    }
  )
  
)