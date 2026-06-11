# Defaults for OptimizerMbo

The following defaults are set for
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md)
during optimization if the respective fields are not set during
initialization.

- **Loop Function**
  ([default_loop_function](https://mlr3mbo.mlr-org.com/dev/reference/default_loop_function.md)):
  The Bayesian optimization flavor. Defaults to
  [bayesopt_ego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_ego.md)
  for single-objective and
  [bayesopt_smsego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_smsego.md)
  for multi-objective optimization.

- **Surrogate**
  ([default_surrogate](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md)):
  The surrogate model. Uses a Gaussian process
  ([default_gp](https://mlr3mbo.mlr-org.com/dev/reference/default_gp.md))
  for purely numeric parameter spaces without dependencies, and a random
  forest
  ([default_rf](https://mlr3mbo.mlr-org.com/dev/reference/default_rf.md))
  otherwise. For multi-objective optimization, one surrogate learner per
  objective is used.

- **Acquisition Function**
  ([default_acqfunction](https://mlr3mbo.mlr-org.com/dev/reference/default_acqfunction.md)):
  The criterion used to propose future points. Defaults to
  [mlr_acqfunctions_cb](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_cb.md)
  (confidence bound) for synchronous single-objective optimization
  (`lambda = 3` for numeric, `lambda = 1` for mixed spaces),
  [mlr_acqfunctions_smsego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md)
  for synchronous multi-objective optimization, and
  [mlr_acqfunctions_stochastic_cb](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_cb.md)
  for asynchronous single-objective optimization.

- **Acquisition Function Optimizer**
  ([default_acqoptimizer](https://mlr3mbo.mlr-org.com/dev/reference/default_acqoptimizer.md)):
  The optimizer for the acquisition function. Defaults to
  [AcqOptimizerLocalSearch](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizerLocalSearch.md).

- **Result Assigner**
  ([default_result_assigner](https://mlr3mbo.mlr-org.com/dev/reference/default_result_assigner.md)):
  Determines how the final result is assigned. Defaults to
  [ResultAssignerArchive](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners_archive.md).

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqfunction.md),
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqoptimizer.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/dev/reference/default_gp.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/dev/reference/default_loop_function.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/dev/reference/default_result_assigner.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/dev/reference/default_rf.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md)
