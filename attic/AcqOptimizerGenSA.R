# Acquistion Function Optimizer
AcqOptimizerGenSA = R6Class(
  "AcqOptimizerGenSA",
  inherit = AcqOptimizer,

  public = list(
    # public member

    # constructor
    initialize = function(control = list(trace.mat = FALSE)) {
      super$initialize(
        "GenSA", 
        parameters = list(control = control), 
        capabilities = "continous", 
        packages = "GenSA")
      self$storage$par = NULL
    },

    # public methods
    optim = function(acq_function) {

      search_space = acq_function$search_space

      fun = function(x) {
        names(x) = search_space$ids()
        x = list(x = x)
        acq_function$evaluate_as_minimization(data.table(x = list(x)))$acq
      }

      res = GenSA::GenSA(
        par = self$storage$par,
        fn = fun,
        lower = search_space$lower,
        upper = search_space$upper,
        control = self$parameters$control
      )

      # store optimal resolution as start for next run
      self$storage$par = res$par
      data.table(x = list(as.list(res$par)))
    }
  )
)
