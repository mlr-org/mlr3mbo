# Default Result Assigner

Chooses a default result assigner. Defaults to
[ResultAssignerArchive](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_archive.md).

## Usage

``` r
default_result_assigner(instance)
```

## Arguments

- instance:

  ([bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html))  
  An object that inherits from
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

## Value

[ResultAssigner](https://mlr3mbo.mlr-org.com/reference/ResultAssigner.md)

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/reference/default_acqfunction.md),
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/reference/default_acqoptimizer.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/reference/default_gp.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/reference/default_loop_function.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/reference/default_rf.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/reference/default_surrogate.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md)
