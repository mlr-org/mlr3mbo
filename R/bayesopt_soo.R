#FIXME: should we pass the arvive here, too?

# most important controls:
# - the acqf, with its settings
# - the terminator, thats in objective
# - the design?
# - the optimizer?

# and why do we pass some tings in "control" and some separately?

bayesop_soo = function(instance, surrogate, acq_function, acq_optimizer) {
  #FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "Objective")
  assert_r6(surrogate, "AcqFunction")
  assert_r6(acq_function, "AcqFunction")
  assert_r6(acq_optimizer, "AcqOptimizer")
  assert_data_table(design, null.ok = TRUE)

  #FIXME maybe do not have this here, but have a general init helper
  if (instance$archive$n_evals == 0) {
    design = generate_design_lhs(instance$search_space, 4 * instance$search_space$length)$data
    instance$eval_batch(design)
  }

  acq_function$setup(archive) #setup necessary to determine the domain, codomain (for opt direction) of acq function

  repeat {
    xydt = archive$data()
    surrogate$update(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y) #update surrogate model with new data
    
    acq_function$update(archive) # NOTE: necessary becaue we have to dertermine e.g. y_best for ei, there are possible other costy calculations that we just want to do once for each state. We might not want to do these calculation in acq_function$eval_dt() because this can get called several times during the optimization.
    # one more costy example would be AEI, where we ask the surrogate for the mean prediction of the points in the design
    # alternatively the update could be called by the AcqOptimizer (but he should not need to know about the archive, so then the archive also has to live in the AcqFun), 
    xdt = acq_optimizer$optim$(acq_function)
    inst$eval_batch(xdt)
    if (inst$is_terminated || inst$terminator$is_terminated(inst$archive)) break
  }

  return(inst$archive)
}




