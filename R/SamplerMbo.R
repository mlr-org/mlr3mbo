#' @title Sampler Suitable for Combining Hyperband With Model Based Optimization
#'
#' @description
#' This class provides a [paradox::Sampler] that allows for sampling points relying on Model Based Optimization.
#' Using this sampler together with [mlr3hyperband::OptimizerHyperband] or [mlr3hyperband::TunerHyperband] allows for constructing an optimizer
#' that performs a combination of Hyperband and Model Based Optimization similarly as the BOHB algorithm in the context of multifidelity optimization.
#'
#' @export
#' @examples
#'if (requireNamespace("mlr3learners") &
#'    requireNamespace("DiceKriging") &
#'    requireNamespace("rgenoud") &
#'    requireNamespace("mlr3hyperband")) {
#'
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3hyperband)
#'   
#'   # Augmented Branin function
#'   fun = function(xs) {
#'     y = 
#'       (
#'         xs[["x2"]] - ((5.1 / (4 * pi^2)) - 0.1 * (1 - (xs[["fidelity"]] / 100))) *
#'         xs[["x1"]]^2 + (5 / pi) * xs[["x1"]] - 6
#'       ) ^ 2 +
#'       10 * (1 - (1 / (8 * pi))) * cos(xs[["x1"]]) + 10
#'     list(y = y)
#'   }
#'   domain = ps(
#'     x1 = p_dbl(lower = -5, upper = 10),
#'     x2 = p_dbl(lower = 0, upper = 15),
#'     fidelity = p_int(lower = 10, upper = 100, tags = "budget"))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceSingleCrit$new(objective = objective, terminator = trm("none"))
#'
#'   surrogate = default_surrogate(instance)
#'
#'   acq_function = acqf("ei")
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search", batch_size = 100),
#'     terminator = trm("evals", n_evals = 100))
#'
#'   sampler = SamplerMbo$new(
#'     instance = instance,
#'     surrogate = surrogate,
#'     acq_function = acq_function,
#'     acq_optimizer = acq_optimizer)
#'
#'   optimizer = opt("hyperband", eta = 3, sampler = sampler, repetitions = 2)
#'   optimizer$optimize(instance)
#' }
SamplerMbo = R6Class("SamplerMbo", inherit = paradox::Sampler,
  public = list(
    #' @field instance ([bbotk::OptimInstance)\cr
    #'   The instance that is to be optimized.
    instance = NULL,

    #' @field budget_id (`character(1)`)\cr
    #'   Id of the budget parameter part of the search space of the instance (i.e., the single numeric or integer parameter tagged via `"budget"`).
    budget_id = NULL,

    #' @field search_space_sampler ([paradox::Domain])\cr
    #'   Search space the sampler operates on.
    #'   This is the same as the search space of the [bbotk::OptimInstance] to be optimized but excludes the parameter specified via `budget_id`.
    search_space_sampler = NULL,

    #' @field rho (`numeric(1)`)\cr
    #'   Fraction of configurations that should be sampled uniformly at random.
    rho = NULL,

    #' @field Nmin (`integer(1)`)\cr
    #'   Minimum number of observations required for any budget level for the model based proposal mechanism to take action.
    Nmin = NULL,

    #' @field tmp_archive ([bbotk::Archive])\cr
    #'   Temporal archive which is a copy of the archive of the [bbotk::OptimInstance] used for updating the surrogate model within the model based proposal mechanism.
    tmp_archive = NULL,
    
    #' @template field_surrogate
    surrogate = NULL,

    #' @template field_acq_function
    acq_function = NULL,

    #' @field acq_function_domain ([paradox::Domain])\cr
    #'   The domain of the acquisition function within the model based proposal mechanism.
    #'   This is the same as `search_space_sampler` without any potential transformation functions.
    acq_function_domain = NULL,

    #' @template field_acq_optimizer
    acq_optimizer = NULL,

    #' @field sampler_random ([paradox::SamplerUnif])\cr
    #'   The sampler responsible for proposing points uniformly at random based on the fraction specified as `rho`.
    sampler_random = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param instance ([bbotk::OptimInstance)\cr
    #'   The instance that is to be optimized.
    #'   Note that this is required as the model based sampling requires access to the evaluations logged in the [bbotk::Archive].
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @param rho (`numeric(1)`)\cr
    #'   Fraction of configurations that should be sampled uniformly at random.
    #' @param Nmin (`integer(1)`)\cr
    #'   Minimum number of observations required for any budget level for the model based proposal mechanism to take action.
    #'   Default is to use the number of parameters of the search space of the instance + 2.
    initialize = function(instance, surrogate, acq_function, acq_optimizer, rho = 0.25, Nmin = NULL) {
      self$instance = assert_r6(instance, classes = "OptimInstance")
      self$budget_id = self$instance$search_space$ids(tags = "budget")
      if (length(self$budget_id) != 1L) {
        stop('Exactly one parameter must be tagged with "budget."')
      }
      assert_choice(self$instance$search_space$class[[self$budget_id]], c("ParamInt", "ParamDbl"))
      self$search_space_sampler = self$instance$search_space$clone(deep = TRUE)$subset(setdiff(self$instance$search_space$ids(), self$budget_id))
      self$rho = assert_number(rho, lower = 0, upper = 1)
      if (is.null(Nmin)) {
        Nmin = self$instance$search_space$length + 2L  # budget param is in search space therefore d - 1 + 1 which is d
      }
      self$Nmin = assert_int(Nmin, lower = 1L)
      self$tmp_archive = self$instance$archive$clone(deep = TRUE)
      self$surrogate = assert_r6(surrogate, classes = "Surrogate")
      self$acq_function = assert_r6(acq_function, classes = "AcqFunction")
      self$acq_function_domain = self$search_space_sampler$clone(deep = TRUE)
      self$acq_function_domain$trafo = NULL
      self$acq_optimizer = assert_r6(acq_optimizer, classes = "AcqOptimizer")
      self$surrogate$archive = self$tmp_archive
      self$surrogate$cols_x = setdiff(surrogate$cols_x, self$budget_id)
      self$acq_function$surrogate = self$surrogate
      self$acq_function$domain = self$acq_function_domain
      self$acq_optimizer$acq_function = self$acq_function
      self$sampler_random = SamplerUnif$new(self$search_space_sampler)
      super$initialize(self$search_space_sampler)
      # FIXME: check if instance, surrogate, acq_function and acq_optimizer work for the instance
    }
  ),
  private = list(
    .sample = function(n) {
      # we need at the minimum Nmin observations for any budget
      # so if we have less then Nmin in total we surely cannot do a model based proposal
      if (self$instance$archive$n_evals < self$Nmin) {
        return(self$sampler_random$sample(n)$data)
      }
      budget_table = self$instance$archive$data[, .N, by = eval(self$budget_id)][N >= self$Nmin]
      # a fraction of rho of the n candidate points are proposed uniformly at random
      propose_random = runif(n, min = 0, max = 1) <= self$rho
      n_random = sum(propose_random)
      xdt = if (n_random < n) {
        # we order the budget table by the budget id and take the largest budget that has at least Nmin observations
        setorderv(budget_table, cols = self$budget_id, order = -1L)
        data = self$instance$archive$data[get(self$budget_id) == budget_table[1L, ][[self$budget_id]], ]
        # we then do one iteration of BO based on this subset of observations excluding the budget id as a parameter
        self$tmp_archive$data = data
        self$acq_function$surrogate$archive = self$tmp_archive
        self$acq_function$domain = self$acq_function_domain
        self$acq_function$surrogate$update()
        self$acq_function$update()
        self$acq_optimizer$param_set$values$n_candidates = n - n_random
        xdt = self$acq_optimizer$optimize()[, self$search_space_sampler$ids(), with = FALSE]
        xdt_random = self$sampler_random$sample(n_random)$data
        rbind(xdt, xdt_random)
      } else {
        self$sampler_random$sample(n_random)$data
      }
      xdt
    }
  )
)
