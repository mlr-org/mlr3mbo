# Default Loop Function

Chooses a default
[loop_function](https://mlr3mbo.mlr-org.com/reference/loop_function.md),
i.e. the Bayesian Optimization flavor to be used for optimization. For
single-objective optimization, defaults to
[bayesopt_ego](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md).
For multi-objective optimization, defaults to
[bayesopt_smsego](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_smsego.md).

## Usage

``` r
default_loop_function(instance)
```

## Arguments

- instance:

  ([bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html))  
  An object that inherits from
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

## Value

[loop_function](https://mlr3mbo.mlr-org.com/reference/loop_function.md)

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/reference/default_acqfunction.md),
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/reference/default_acqoptimizer.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/reference/default_gp.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/reference/default_result_assigner.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/reference/default_rf.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/reference/default_surrogate.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md)
