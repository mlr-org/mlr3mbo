bayesop_mpcl = function(instance, surrogate, acq_function, acq_optimizer, liar, q) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceSingleCrit")
  assert_r6(surrogate, "Surrogate")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_function(liar)
  assert_int(q)

  archive = instance$archive

  #FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = generate_design_lhs(instance$search_space, 4 * instance$search_space$length)$data
    instance$eval_batch(design)
  }

  acq_function$setup(archive) #setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    proposals = data.table()
    temp_archive = archive$clone(deep = TRUE)
    temp_acq_function = acq_function$clone(deep = TRUE)
    lie = data.table(liar(archive$data()[[archive$cols_y]]))
    colnames(lie) = archive$cols_y

    for(i in seq_len(q)) {
      xdt = acq_optimizer$optimize(temp_acq_function) # temp_acq_fuunction?
      proposals = rbind(proposals, xdt)
      temp_archive$add_evals(xdt, transform_xdt_to_xss(xdt, temp_archive$search_space), lie)
      temp_acq_function$update(temp_archive)
    }

    instance$eval_batch(proposals)
    acq_function$update(instance$archive)

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

  surrogate = SurrogateLearner$new(learner = lrn("regr.ranger"))
  acqfun = AcqFunctionEI$new(surrogate = surrogate)
  acqopt = AcqOptimizerRandomSearch$new()
  proposal_generator = ProposalGeneratorSingle$new(
    acq_function = acqfun,
    acq_optimizer = acqopt)

  bayesop_mpcl(instance, surrogate, acqfun, acqopt, mean, 2)
}



