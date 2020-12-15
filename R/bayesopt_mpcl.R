bayesop_mpcl = function(instance, acq_function, acq_optimizer, liar, q) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceSingleCrit")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_function(liar)
  assert_int(q, lower = 2L)

  archive = instance$archive

  #FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = generate_design_lhs(instance$search_space, 4 * instance$search_space$length)$data
    instance$eval_batch(design)
  }

  acq_function$setup(archive) #setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    # normal soo updates
    acq_function$surrogate$update(xydt = archive_xy(archive), y_cols = archive$cols_y)
    acq_function$update(archive)

    acq_function$update(instance$archive)
    xdt = acq_optimizer$optimize(acq_function)

    # prepare lie objects
    temp_archive = MboDummyArchive$new(archive)
    temp_acq_function = acq_function$clone(deep = TRUE) # also generates clone of the surrogate, should be the only reason why we clone the acqfun
    lie = data.table(liar(archive$data()[[archive$cols_y]]))
    colnames(lie) = archive$cols_y
    xdt_new = xdt

    # obtain proposals, fill with fake archive lie
    for (i in seq(2, q)) {
      # add lie instead of true eval
      temp_archive$add_evals(xdt = xdt_new, ydt = lie)

      # update all objects with lie
      temp_acq_function$surrogate$update(xydt = archive_xy(temp_archive),, y_cols = temp_archive$cols_y)

      # obtain new proposal based on lie
      temp_acq_function$update(temp_archive)
      xdt_new = acq_optimizer$optimize(temp_acq_function)
      xdt = rbind(xdt, xdt_new)
    }

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
    fun = function(xs) sum(unlist(xs)^2),
    domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
    id = "test"
  )

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.ranger"))
  acqfun = AcqFunctionEI$new(surrogate = surrogate)
  acqopt = AcqOptimizerRandomSearch$new()

  bayesop_mpcl(instance, acqfun, acqopt, mean, 2)
  data = instance$archive$data()
  plot(y~batch_nr, data[batch_nr>1,], type = "b")

  xgrid = generate_design_grid(instance$search_space, 100)$data
  preds = cbind(xgrid, acqfun$surrogate$predict(xgrid))
  library(ggplot2)
  g = ggplot(data, aes(x = x, y = y, col = batch_nr))
  g = g + geom_line() + geom_point()
  g = g + geom_line(data = preds, aes(x = x, y = mean), col = "blue")
  g
}
