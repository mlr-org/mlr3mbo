

bayesop_smsemoa = function(instance, surrogate, acq_function, acq_optimizer) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_r6(surrogate, "SurrogateMultiCrit")
  assert_r6(acq_function, "AcqFunctionSmsEmoa")
  assert_r6(acq_optimizer, "AcqOptimizer")

  archive = instance$archive
  #FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = generate_design_lhs(instance$search_space, 4 * instance$search_space$length)$data
    instance$eval_batch(design)
  }

  acq_function$progress = instance$terminator$param_set$values$n_evals - archive$n_evals
  acq_function$setup(archive) #setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    xydt = archive$data()
    surrogate$update(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y) #update surrogate model with new data
    acq_function$progress = instance$terminator$param_set$values$n_evals - archive$n_evals
    acq_function$update(archive)
    xdt = acq_optimizer$optimize(acq_function)
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
    list(y1 = xs[[1]]^2, y2 = -xs[[2]]^2)
  }
  PS_2D = ParamSet$new(list(
    ParamDbl$new("x1", lower = -1, upper = 1),
    ParamDbl$new("x2", lower = -1, upper = 1)
  ))
  FUN_2D_2D_CODOMAIN = ParamSet$new(list(
    ParamDbl$new("y1", tags = "minimize"),
    ParamDbl$new("y2", tags = "maximize")
  ))
  obfun = ObjectiveRFun$new(fun = FUN_2D_2D, domain = PS_2D,
    codomain = FUN_2D_2D_CODOMAIN, properties = "multi-crit")

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceMultiCrit$new(
    objective = obfun, 
    terminator = terminator
  )

  surrogate = SurrogateMultiCritLearners$new(learners = replicate(2, lrn("regr.km")))
  acqfun = AcqFunctionSmsEmoa$new(surrogate = surrogate)
  acqopt = AcqOptimizerRandomSearch$new()

  bayesop_smsemoa(instance, surrogate, acqfun, acqopt)

  archdata = instance$archive$data()
  library(ggplot2)
  g = ggplot(archdata, aes_string(x = "y1", y = "y2", color = "batch_nr"))
  g + geom_point()
}




