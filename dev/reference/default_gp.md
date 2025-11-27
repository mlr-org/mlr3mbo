# Default Gaussian Process

This is a helper function that constructs a default Gaussian Process
[mlr3learners::LearnerRegrKM](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.km.html)
which is for example used in
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md).

Constructs a Kriging learner `"regr.km"` with kernel `"matern5_2"`. If
`noisy = FALSE` (default) a small nugget effect is added
`nugget.stability = 10^-8` to increase numerical stability to hopefully
prevent crashes of `DiceKriging`. If `noisy = TRUE` the nugget effect
will be estimated with `nugget.estim = TRUE`. If `noisy = TRUE` `jitter`
is set to `TRUE` to circumvent a problem with `DiceKriging` where
already trained input values produce the exact trained output. In
general, instead of the default `"BFGS"` optimization method we use
rgenoud (`"gen"`), which is a hybrid algorithm, to combine global search
based on genetic algorithms and local search based on gradients. This
may improve the model fit and will less frequently produce a constant
model prediction.

## Usage

``` r
default_gp(noisy = FALSE)
```

## Arguments

- noisy:

  (`logical(1)`)  
  Whether the learner will be used in a noisy objective function
  scenario. See above.

## Value

[mlr3learners::LearnerRegrKM](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.km.html)

## See also

Other mbo_defaults:
[`default_acqfunction()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqfunction.md),
[`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/dev/reference/default_acqoptimizer.md),
[`default_loop_function()`](https://mlr3mbo.mlr-org.com/dev/reference/default_loop_function.md),
[`default_result_assigner()`](https://mlr3mbo.mlr-org.com/dev/reference/default_result_assigner.md),
[`default_rf()`](https://mlr3mbo.mlr-org.com/dev/reference/default_rf.md),
[`default_surrogate()`](https://mlr3mbo.mlr-org.com/dev/reference/default_surrogate.md),
[`mbo_defaults`](https://mlr3mbo.mlr-org.com/dev/reference/mbo_defaults.md)
