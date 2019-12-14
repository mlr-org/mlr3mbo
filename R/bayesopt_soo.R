#FIXME: should we pass the arvive here, too?

# most important controls:
# - the acqf, with its settings
# - the terminator, thats in objective
# - the design?
# - the optimizer?

# and why do we pass some tings in "control" and some separately?

bayesop_soo = function(objective, learner, acqf, terminator, init_random = NULL) {
  assert_r6(obj, "Objective")
  assert_r6(acqf, "AcqFunction")
  assert_r6(acqf_optim, "AcqOptimizer")
  archive = Archive$new(objective$domain, objective$codomain)
  sm = Surrogate$new(learner)

  # FIXME: where do we have the init design set?
  ps = obj$domain
  if (is.null(init_random)) {
    # FIXME: magic constant
    init_random = 4 * ps$length
  }
  ev = Evaluator$new(obj, archive, term)
  if (init_random > 0L) {
    d = generate_design_random(ps, init_random)
    ev$eval_points(d$data)
  }
  acqf_optim = AcqOptimizer$new()
  while(!terminator$is_terminated(archive)) {
    # FIXME: maybe have a function to eval all unevalued poitbs in the archive?
    sm$train(archive)
    acqf$set_up(ps, archive, sm)
    # FIXME: magic constant
    x = acqf_optim$optimize(acqf, n_evals = 3) 
    # FIXME: use proposer
    ev$eval_points(x)
  }
  return(archive)
}




