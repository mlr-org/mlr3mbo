bayesopt_mpcl = function(instance, acq_function = NULL, acq_optimizer = NULL, liar, q) {
  assert_r6(instance, "OptimInstanceSingleCrit")
  if (is.null(acq_function)) {
    surrogate = default_surrogate(instance)
    acq_function = default_acqfun(instance, surrogate = surrogate)
  }
  if (is.null(acq_optimizer)) {
    acq_optimizer = default_acqoptimizer(instance)
  }
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_function(liar)
  assert_int(q, lower = 2L)

  eval_initial_design(instance)
  archive = instance$archive
  acq_function$setup(archive) # setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    # normal soo updates with error catching
    xdt = tryCatch({
      acq_function$update_surrogate(archive)
      acq_function$update(archive)
      acq_optimizer$optimize(acq_function, archive = archive)  # archive need for fix_xdt_distance()
    }, leads_to_exploration_error = function(leads_to_exploration_error_condition) {
      lg$info("Proposing a randomly sampled point")  # FIXME: logging?
      SamplerUnif$new(instance$search_space)$sample(1L)$data  # FIXME: also think about augmented lhs
    })

    # prepare lie objects
    temp_archive = archive$clone(deep = TRUE)
    temp_acq_function = acq_function$clone(deep = TRUE) # also generates clone of the surrogate, should be the only reason why we clone the acqfun
    lie = data.table(liar(archive$data[[archive$cols_y]]))
    colnames(lie) = archive$cols_y
    xdt_new = xdt

    # obtain proposals, fill with fake archive lie, also with error catching
    for (i in seq(2, q)) {
      xdt_new = tryCatch({
        # add lie instead of true eval
        temp_archive$add_evals(xdt = xdt_new, transform_xdt_to_xss(xdt_new, temp_archive$search_space), ydt = lie)

        # update all objects with lie
        temp_acq_function$surrogate$update(xydt = archive_xy(temp_archive), y_cols = temp_archive$cols_y)

        # obtain new proposal based on lie
        temp_acq_function$update(temp_archive)
        acq_optimizer$optimize(temp_acq_function, archive = archive)
      }, leads_to_exploration_error = function(leads_to_exploration_error_condition) {

        lg$info("Proposing a randomly sampled point")  # FIXME: logging?
        SamplerUnif$new(instance$search_space)$sample(1L)$data  # FIXME: also think about augmented lhs
      })
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
    fun = function(xs) list(y = sum(unlist(xs)^2)),
    domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
    id = "test"
  )

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.ranger"))
  acq_function = AcqFunctionEI$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000))

  bayesopt_mpcl(instance, acq_function, acq_optimizer, mean, 2)

  # Defaults work
  bayesopt_mpcl(instance, liar = mean, q = 2L)

  data = instance$archive$data
  plot(y~batch_nr, data[batch_nr>1,], type = "b")

  xgrid = generate_design_grid(instance$search_space, 100)$data
  preds = cbind(xgrid, acq_function$surrogate$predict(xgrid))
  library(ggplot2)
  g = ggplot(data, aes(x = x, y = y, col = batch_nr))
  g = g + geom_line() + geom_point()
  g = g + geom_line(data = preds, aes(x = x, y = mean), col = "blue")
  g
}
