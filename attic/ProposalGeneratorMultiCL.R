ProposalGeneratorMultiCL = R6Class(
  "ProposalGeneratorMultiCL",
  inherit = ProposalGeneratorMulti,
  
  public = list(
    # public member
    acq_function = NULL,
    acq_optimizer = NULL,
    lie = NULL,
    
    # constructor
    initialize = function(acq_function, acq_optimizer, n = 1, lie = "min") {
      super$initialize(n = n)
      self$acq_function = acq_function
      self$acq_optimizer = acq_optimizer
      self$lie = assert_choice(lie, c("min", "max", "mean"))
    },
    
    # public methods
    # value: data.table with column x (plus additional stuff in additional columns?)
    generate = function(opt_state) {
      this_opt_state = opt_state$clone(deep = TRUE) # FIXME This is probably not very fast, because we copy a (potentially big) surrogate model which we probably wont use. But this depends on the surrogate and should be handled there I guess.
      lie_fun = get(self$lie, mode = "function")
      proposals = vector("list", self$n)
      for (i in seq_len(self$n)) {
        self$acq_function$prepare(this_opt_state)
        prop = self$acq_optimizer$optim(self$acq_function)
        proposals[[i]] = prop
        prop$y = lie_fun(this_opt_state$surrogate_model$design$y)
        this_opt_state$surrogate_model$add(prop)
      }
      rbindlist(proposals)
    }
  )
  
)