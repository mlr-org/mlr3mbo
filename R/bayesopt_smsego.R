#' @title Multi-Objective SMS-MBO
#'
#' @description
#' Function that executes a SMS multi-objective Bayesian optimization.
#' @template param_instance
#' @template param_acq_function
#' @template param_acq_optimizer
#' @return [bbotk::Archive]
#' @export
bayesopt_smsego = function(instance, acq_function = NULL, acq_optimizer = NULL) {
  assert_r6(instance, "OptimInstanceMultiCrit")
  if (is.null(acq_function)) {
    surrogate = default_surrogate(instance)
    acq_function = AcqFunctionSmsEgo$new(surrogate = surrogate)
  }
  if (is.null(acq_optimizer)) {
    acq_optimizer = default_acqopt(instance)
  }
  assert_r6(acq_function, "AcqFunctionSmsEgo")
  assert_r6(acq_optimizer, "AcqOptimizer")

  eval_initial_design(instance)
  archive = instance$archive
  acq_function$setup(archive) # setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    xdt = tryCatch({
      # FIXME:
      acq_function$update_surrogate(archive)
      acq_function$progress = instance$terminator$param_set$values$n_evals - archive$n_evals
      acq_function$update(archive)
      acq_optimizer$optimize(acq_function)
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
  devtools::load_all()
  library(bbotk)
  library(paradox)
  library(mlr3learners)

  FUN_2D_2D = function(xs) {
    list(y1 = xs$x1 ^ 2, y2 = (xs$x1 - 2) ^ 2)
  }
  PS_2D = ParamSet$new(list(
    ParamDbl$new("x1", lower = -10, upper = 10)
  ))
  FUN_2D_2D_CODOMAIN = ParamSet$new(list(
    ParamDbl$new("y1", tags = "minimize"),
    ParamDbl$new("y2", tags = "minimize")
  ))
  obfun = ObjectiveRFun$new(fun = FUN_2D_2D, domain = PS_2D,
    codomain = FUN_2D_2D_CODOMAIN, properties = "multi-crit")

  terminator = trm("evals", n_evals = 30)

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateMultiCritLearners$new(learners = replicate(2, lrn("regr.ranger")))
  acq_function = AcqFunctionSmsEgo$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000))

  bayesopt_smsego(instance, acq_function, acq_optimizer)
  # Defaults work
  bayesopt_smsego(instance)

  archdata = instance$archive$data
  library(ggplot2)
  g = ggplot(archdata, aes_string(x = "y1", y = "y2", color = "batch_nr"))
  g = g + geom_point()
  g + geom_point(data = best, color = "red")
}

