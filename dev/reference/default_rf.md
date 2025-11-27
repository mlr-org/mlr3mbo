# Default Random Forest

This is a helper function that constructs a default random forest
[mlr3learners::LearnerRegrRanger](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.ranger.html)
which is for example used in
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md).

## Usage

``` r
default_rf(noisy = FALSE)
```

## Arguments

- noisy:

  (`logical(1)`)  
  Whether the learner will be used in a noisy objective function
  scenario. See
  [`default_surrogate()`](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md).

## Value

[mlr3learners::LearnerRegrRanger](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.ranger.html)

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqfunction.md),
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqoptimizer.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/dev/reference/default_gp.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/dev/reference/default_loop_function.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/dev/reference/default_result_assigner.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/dev/reference/mbo_defaults.md)
