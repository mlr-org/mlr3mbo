# Acquisition Function Augmented Expected Improvement

Augmented Expected Improvement. Useful when working with noisy
objectives. Currently only works correctly with `"regr.km"` as surrogate
model and `nugget.estim = TRUE` or given.

## Dictionary

This
[AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/dev/reference/acqf.md):

    mlr_acqfunctions$get("aei")
    acqf("aei")

## Parameters

- `"c"` (`numeric(1)`)  
  Constant \\c\\ as used in Formula (14) of Huang (2012) to reflect the
  degree of risk aversion. Defaults to `1`.

## References

- Huang D, Allen TT, Notz WI, Zheng N (2012). “Erratum To: Global
  Optimization of Stochastic Black-box Systems via Sequential Kriging
  Meta-Models.” *Journal of Global Optimization*, **54**(2), 431–431.

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ei.md),
[`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ei_log.md),
[`mlr_acqfunctions_eips`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_eips.md),
[`mlr_acqfunctions_mean`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_mean.md),
[`mlr_acqfunctions_multi`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_multi.md),
[`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_pi.md),
[`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_sd.md),
[`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md),
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_ei.md)

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3mbo::AcqFunction`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
-\> `AcqFunctionAEI`

## Public fields

- `y_effective_best`:

  (`numeric(1)`)  
  Best effective objective value observed so far. In the case of
  maximization, this already includes the necessary change of sign.

- `noise_var`:

  (`numeric(1)`)  
  Estimate of the variance of the noise. This corresponds to the
  `nugget` estimate when using a
  [mlr3learners](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.km.html)
  as surrogate model.

## Methods

### Public methods

- [`AcqFunctionAEI$new()`](#method-AcqFunctionAEI-new)

- [`AcqFunctionAEI$update()`](#method-AcqFunctionAEI-update)

- [`AcqFunctionAEI$clone()`](#method-AcqFunctionAEI-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3mbo::AcqFunction$eval_dt()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.html#method-eval_dt)
- [`mlr3mbo::AcqFunction$eval_many()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.html#method-eval_many)
- [`mlr3mbo::AcqFunction$reset()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.html#method-reset)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqFunctionAEI$new(surrogate = NULL, c = 1)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)).

- `c`:

  (`numeric(1)`).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function and set `y_effective_best` and
`noise_var`.

#### Usage

    AcqFunctionAEI$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionAEI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {
  library(bbotk)
  library(paradox)
  library(mlr3learners)
  library(data.table)

  set.seed(2906)
  fun = function(xs) {
    list(y = xs$x ^ 2 + rnorm(length(xs$x), mean = 0, sd = 1))
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun,
    domain = domain,
    codomain = codomain,
    properties = "noisy")

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))

  learner = lrn("regr.km",
    covtype = "matern5_2",
    optim.method = "gen",
    nugget.estim = TRUE,
    jitter = 1e-12,
    control = list(trace = FALSE))

  surrogate = srlrn(learner, archive = instance$archive)

  acq_function = acqf("aei", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>     acq_aei
#>       <num>
#> 1: 7.583607
#> 2: 7.583607
#> 3: 7.583607
```
