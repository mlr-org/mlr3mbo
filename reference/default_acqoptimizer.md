# Default Acquisition Function Optimizer

Chooses a default acquisition function optimizer. Defaults to
[AcqOptimizerLocalSearch](https://mlr3mbo.mlr-org.com/reference/AcqOptimizerLocalSearch.md)
with `n_searches = 10`, `n_steps = ceiling(100 * D^2 / 300)`, and
`n_neighs = 30`, where `D` is the dimension of the search space.

## Usage

``` r
default_acqoptimizer(acq_function, instance)
```

## Arguments

- acq_function:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

- instance:

  ([bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)).

## Value

[AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/reference/default_acqfunction.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/reference/default_gp.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/reference/default_loop_function.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/reference/default_result_assigner.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/reference/default_rf.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/reference/default_surrogate.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md)
