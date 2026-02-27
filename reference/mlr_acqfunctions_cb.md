# Acquisition Function Confidence Bound

Lower / Upper Confidence Bound.

## Dictionary

This [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md):

    mlr_acqfunctions$get("cb")
    acqf("cb")

## Parameters

- `"lambda"` (`numeric(1)`)  
  \\\lambda\\ value used for the confidence bound. Defaults to `2`.

## References

- Snoek, Jasper, Larochelle, Hugo, Adams, P R (2012). “Practical
  Bayesian Optimization of Machine Learning Algorithms.” In Pereira F,
  Burges CJC, Bottou L, Weinberger KQ (eds.), *Advances in Neural
  Information Processing Systems*, volume 25, 2951–2959.

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md),
[`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei_log.md),
[`mlr_acqfunctions_eips`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_eips.md),
[`mlr_acqfunctions_mean`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_mean.md),
[`mlr_acqfunctions_multi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_multi.md),
[`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_pi.md),
[`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.md),
[`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md),
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_ei.md)

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3mbo::AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
-\> `AcqFunctionCB`

## Methods

### Public methods

- [`AcqFunctionCB$new()`](#method-AcqFunctionCB-new)

- [`AcqFunctionCB$clone()`](#method-AcqFunctionCB-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3mbo::AcqFunction$eval_dt()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-eval_dt)
- [`mlr3mbo::AcqFunction$eval_many()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-eval_many)
- [`mlr3mbo::AcqFunction$reset()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-reset)
- [`mlr3mbo::AcqFunction$update()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-update)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqFunctionCB$new(surrogate = NULL, lambda = 2)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)).

- `lambda`:

  (`numeric(1)`).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionCB$clone(deep = FALSE)

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

  fun = function(xs) {
    list(y = xs$x ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))

  learner = default_gp()

  surrogate = srlrn(learner, archive = instance$archive)

  acq_function = acqf("cb", surrogate = surrogate, lambda = 3)

  acq_function$surrogate$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>       acq_cb
#>        <num>
#> 1: -55.38846
#> 2: -55.57171
#> 3: -49.63773
```
