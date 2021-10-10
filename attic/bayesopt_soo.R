# FIXME: should we pass the arvive here, too?

# most important controls:
# - the acqf, with its settings
# - the terminator, thats in objective
# - the design?
# - the optimizer?


#' @title Single Objective Bayesian Optimization
#'
#' @description
#' Function that executes a simple single-objective Bayesian optimization.
#' @template param_instance
#' @template param_acq_function
#' @template param_acq_optimizer
#' @param n_design (`integer(1)`)\cr
#'   In case the `archive` inside the `instance` is empty, we generate a random initial design of `n_design` points.
#' @return [bbotk::Archive].
#'
#' @references
#' `r format_bib("jones_1998")`
#'
#' @export
bayesopt_soo = function(instance, surrogate = NULL, acq_function = NULL, acq_optimizer = NULL, n_design = 4 * instance$search_space$length) {
  assert_r6(instance, "OptimInstance")
  if (is.null(acq_function)) {
    surrogate = default_surrogate(instance)
    acq_function = default_acqfun(instance, surrogate = surrogate)
  }
  if (is.null(acq_optimizer)) {
    acq_optimizer = default_acqopt(instance)
  }
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")

  eval_initial_design(instance)
  archive = instance$archive

  repeat {
    xdt = tryCatch({
    surrogate(archive)
      acq_function$update()
      acq_optimizer$optimize(acq_function, archive = archive)  # archive need for fix_xdt_distance()
    }, leads_to_exploration_error = function(leads_to_exploration_error_condition) {
      lg$info("Proposing a randomly sampled point")  # FIXME: logging?
      SamplerUnif$new(instance$search_space)$sample(1L)$data  # FIXME: also think about augmented lhs
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(archive)) break
  }

  return(instance$archive)
}

if (FALSE) {
  set.seed(1)
  library(bbotk)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  obfun = ObjectiveRFun$new(
    fun = function(xs) list(y = sum(unlist(xs)^2)),
    domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
    id = "test"
  )

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  acq_function = AcqFunctionEI$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000))

  bayesopt_soo(instance, acq_function, acq_optimizer)

  # Defaults work
  bayesopt_soo(instance)
}
