# Default Surrogate

This is a helper function that constructs a default
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)
based on properties of the
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

For purely numeric (including integers) parameter spaces without any
dependencies a Gaussian Process is constricted via
[`default_gp()`](https://mlr3mbo.mlr-org.com/dev/reference/default_gp.md).
For mixed numeric-categorical parameter spaces, or spaces with
conditional parameters a random forest is constructed via
[`default_rf()`](https://mlr3mbo.mlr-org.com/dev/reference/default_rf.md).

In any case, learners are encapsulated using `"evaluate"`, and a
fallback learner is set, in cases where the surrogate learner errors.
Currently, the following learner is used as a fallback:
`lrn("regr.ranger", num.trees = 10L, keep.inbag = TRUE, se.method = "jack")`.

If additionally dependencies are present in the parameter space,
inactive conditional parameters are represented by missing `NA` values
in the training design data. We simply handle those with the internal
`NA` handling method `na.action = "na_learn` of
[ranger](https://CRAN.R-project.org/package=ranger).

If `n_learner` is `1`, the learner is wrapped as a
[SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md).
Otherwise, if `n_learner` is larger than `1`, multiple deep clones of
the learner are wrapped as a
[SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearnerCollection.md).

## Usage

``` r
default_surrogate(
  instance,
  learner = NULL,
  n_learner = NULL,
  force_random_forest = FALSE
)
```

## Arguments

- instance:

  ([bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html))  
  An object that inherits from
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

- learner:

  (`NULL` \|
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)). If
  specified, this learner will be used instead of the defaults described
  above.

- n_learner:

  (`NULL` \| `integer(1)`). Number of learners to be considered in the
  construction of the
  [Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md).
  If not specified will be based on the number of objectives as stated
  by the instance.

- force_random_forest:

  (`logical(1)`). If `TRUE`, a random forest is constructed even if the
  parameter space is purely numeric.

## Value

[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqfunction.md),
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqoptimizer.md),
[`default_gp()`](https://mlr3mbo.mlr-org.com/dev/reference/default_gp.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/dev/reference/default_loop_function.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/dev/reference/default_result_assigner.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/dev/reference/default_rf.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/dev/reference/mbo_defaults.md)
