# FIXME: should we pass the arvive here, too?

# most important controls:
# - the acqf, with its settings
# - the terminator, thats in objective
# - the design?
# - the optimizer?

# and why do we pass some tings in "control" and some separately?

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
bayesopt_soo = function(instance, acq_function, acq_optimizer, n_design = 4 * instance$search_space$length) {
  # FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstance")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  archive = instance$archive

  # FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = if(instance$search_space$has_deps) {
      generate_design_random(instance$search_space, n_design)$data
    } else {
      generate_design_lhs(instance$search_space, n_design)$data
    }
    instance$eval_batch(design)
  }

  acq_function$setup(archive) # setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    xdt = tryCatch({
      acq_function$surrogate$update(xydt = archive_xy(archive), y_cols = archive$cols_y)  # update surrogate model with new data

      # NOTE: necessary because we have to determine e.g. y_best for ei.
      # There are possible other costy calculations that we just want to do once for each state.
      # We might not want to do these calculation in acq_function$fun() because this can get called several times during the optimization.
      # One more costy example would be AEI, where we ask the surrogate for the mean prediction of the points in the design.
      # Alternatively the update could be called by the AcqOptimizer (but he should not need to know about the archive, so then the archive also has to live in the AcqFunction).
      acq_function$update(archive)

      xdt = acq_optimizer$optimize(acq_function, archive = archive)  # archive need for fix_xdt_distance()
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
}
