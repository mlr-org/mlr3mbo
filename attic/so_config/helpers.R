make_optim_instance = function(instance) {
  benchmark = BenchmarkSet$new(as.character(instance$scenario), instance = as.character(instance$instance))
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(as.character(instance$instance), multifidelity = FALSE, check_values = FALSE)
  n_evals = as.integer(ceiling(instance$budget / instance$max_budget))  # full budget
  optim_instance = OptimInstanceSingleCrit$new(objective, search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = n_evals), check_values = FALSE)
  optim_instance
}

