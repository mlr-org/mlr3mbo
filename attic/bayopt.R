# main routine

bayopt = function(opt_problem) {

  opt_state = opt_problem$init()

  while (opt_state$can_continue()) {
    opt_state$step()
  }

  opt_state$terminate()
}
