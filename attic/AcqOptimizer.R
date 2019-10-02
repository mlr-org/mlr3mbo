# Acquistion Function Optimizer
AcqOptimizer = R6Class(
  "AcqOptimizer",

  public = list(
    # public member
    id = character(1L),
    parameters = list(),
    storage = list(),
    capabilities = character(),
    packages = character(),

    # constructor
    initialize = function(id, parameters, capabilities, packages) {
      self$id = id
      self$parameters = parameters
      self$capabilities = capabilities
      self$packages = packages
    },

    # public methods

    # optimizes the acq_function
    # returns a data.table with a list column x
    optim = function(acq_function) {
      stop("Not implemented.")
    }
  )
)
