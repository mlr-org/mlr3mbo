bayesop_parego = function(instance, acq_function, acq_optimizer, q = 1, s = 100, rho = 0.05) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_int(q, lower = 1)

  archive = instance$archive
  #FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = generate_design_lhs(instance$search_space, 4 * instance$search_space$length)$data
    instance$eval_batch(design)
  }

  # ParEGO Archive
  dummy_codomain = ParamSet$new(list(ParamDbl$new("y_scal", tags = "minimize")))
  dummy_archive = MboDummyArchive$new(archive, codomain = dummy_codomain)
  #manual fix: #FIXME Write ParEGO Infill Crit? 
  acq_function$setup(dummy_archive)
  
  repeat {
    xydt = archive$data()
    xdt = xydt[, archive$cols_x, with = FALSE]
    ydt = xydt[, archive$cols_y, with = FALSE]
    d = archive$codomain$length

    ydt = Map("*", ydt, mult_max_to_min(archive$codomain))

    xdt = map_dtr(seq_len(q), function(i) {
      # FIXME: Wrong way to calculate lambda
      lambda = runif(d)
      lambda = lambda / sum(lambda)
      
      mult = Map('*', ydt, lambda)
      y_scal = do.call('+', mult)
      dummy_archive$clear()
      dummy_archive$add_cols(data.table(y_scal = y_scal))

      xydt = dummy_archive$data()
      acq_function$surrogate$setup(xydt = xydt[, c(dummy_archive$cols_x, dummy_archive$cols_y), with = FALSE], y_cols = dummy_archive$cols_y) #update surrogate model with new data
      acq_function$update(dummy_archive)
      acq_optimizer$optimize(acq_function)  
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(instance$archive)) break
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

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  acq_function = AcqFunctionEI$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizerRandomSearch$new()

  bayesop_parego(instance, acq_function, acq_optimizer, q = 2)

  archdata = instance$archive$data()
  library(ggplot2)
  g = ggplot(archdata, aes_string(x = "y1", y = "y2", color = "batch_nr"))
  g + geom_point()
}




