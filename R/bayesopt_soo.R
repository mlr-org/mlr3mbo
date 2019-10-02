#FIXME: should we pass the arvive here, too?

# most important controls:
# - the acqf, with its settings
# - the terminator, thats in objective
# - the design?
# - the optimizer?

# and why do we pass some tings in "control" and some separately?

bayesop_soo = function(obj, learner, acqf, acqf_optim, init_random = NULL) {
  assert_r6(obj, "Objective")
  assert_r6(acqf, "AcqFunction")
  assert_r6(acqf_optim, "AcqOptimizer")
  # FIXME: where do we have the init design set?
  ps = obj$param_set
  if (is.null(init_random)) {
    init_random = 4 * ps$length
  }
  if (init_random > 0L) {
    d = generate_design_random(ps, init_random)
    dd = d$data
    obj$eval_batch(dd)
  }
  acqf_optim = AcqOptimizer$new()
  while(!obj$terminator$is_terminated(obj)) {
    # FIXME: maybe have a function to eval all unevalued poitbs in the archive?
    # FIXME: put this in objective? not good if in opttools. or in helper? surroaget?
    task = TaskRegr$new(backend = obj$archive$data, target = "y", id = "surrogate_task")
    ll$train(task)
    acqf$set_up(ps, task, ll)
    x = acqf_optim$optimize(acqf, n_evals = 3) 
    obj$eval_batch(dd)
    dd = rbind(dd, x)
  }
}




