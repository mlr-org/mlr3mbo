# Default Acquisition Function

Chooses a default acquisition function, i.e. the criterion used to
propose future points. For synchronous single-objective optimization and
a purely numeric parameter space, defaults to
[mlr_acqfunctions_cb](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md)
with `lambda = 3`. For synchronous single-objective optimization and a
mixed numeric-categorical parameter space, defaults to
[mlr_acqfunctions_cb](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md)
with `lambda = 1`. For synchronous multi-objective optimization,
defaults to
[mlr_acqfunctions_smsego](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md).
For asynchronous single-objective optimization, defaults to
[mlr_acqfunctions_stochastic_cb](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md).

## Usage

``` r
default_acqfunction(instance)
```

## Arguments

- instance:

  ([bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)).
  An object that inherits from
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

## Value

[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)

## See also

Other mbo_defaults:
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/reference/default_acqoptimizer.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/reference/default_gp.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/reference/default_loop_function.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/reference/default_result_assigner.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/reference/default_rf.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/reference/default_surrogate.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md)
