# Acquisition Function Stochastic Confidence Bound

Lower / Upper Confidence Bound with lambda sampling and decay. The
initial \\\lambda\\ is drawn from an uniform distribution between
`min_lambda` and `max_lambda` or from an exponential distribution with
rate `1 / lambda`. \\\lambda\\ is updated after each update by the
formula `lambda * exp(-rate * (t %% period))`, where `t` is the number
of times the acquisition function has been updated.

While this acquisition function usually would be used within an
asynchronous optimizer, e.g.,
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_async_mbo.md),
it can in principle also be used in synchronous optimizers, e.g.,
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md).

## Dictionary

This
[AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/dev/reference/acqf.md):

    mlr_acqfunctions$get("stochastic_cb")
    acqf("stochastic_cb")

## Parameters

- `"lambda"` (`numeric(1)`)  
  \\\lambda\\ value for sampling from the exponential distribution.
  Defaults to `1.96`.

- `"min_lambda"` (`numeric(1)`)  
  Minimum value of \\\lambda\\for sampling from the uniform
  distribution. Defaults to `0.01`.

- `"max_lambda"` (`numeric(1)`)  
  Maximum value of \\\lambda\\ for sampling from the uniform
  distribution. Defaults to `10`.

- `"distribution"` (`character(1)`)  
  Distribution to sample \\\lambda\\ from. One of
  `c("uniform", "exponential")`. Defaults to `uniform`.

- `"rate"` (`numeric(1)`)  
  Rate of the exponential decay. Defaults to `0` i.e. no decay.

- `"period"` (`integer(1)`)  
  Period of the exponential decay. Defaults to `NULL`, i.e., the decay
  has no period.

## Note

- This acquisition function always also returns its current
  (`acq_lambda`) and original (`acq_lambda_0`) \\\lambda\\. These values
  will be logged into the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  of the
  [bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
  of the
  [AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
  and therefore also in the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  that is to be optimized.

## References

- Snoek, Jasper, Larochelle, Hugo, Adams, P R (2012). “Practical
  Bayesian Optimization of Machine Learning Algorithms.” In Pereira F,
  Burges CJC, Bottou L, Weinberger KQ (eds.), *Advances in Neural
  Information Processing Systems*, volume 25, 2951–2959.

- Egelé, Romain, Guyon, Isabelle, Vishwanath, Venkatram, Balaprakash,
  Prasanna (2023). “Asynchronous Decentralized Bayesian Optimization for
  Large Scale Hyperparameter Optimization.” In *2023 IEEE 19th
  International Conference on e-Science (e-Science)*, 1–10.

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_aei.md),
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
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_ei.md)

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3mbo::AcqFunction`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
-\> `AcqFunctionStochasticCB`

## Methods

### Public methods

- [`AcqFunctionStochasticCB$new()`](#method-AcqFunctionStochasticCB-new)

- [`AcqFunctionStochasticCB$update()`](#method-AcqFunctionStochasticCB-update)

- [`AcqFunctionStochasticCB$reset()`](#method-AcqFunctionStochasticCB-reset)

- [`AcqFunctionStochasticCB$clone()`](#method-AcqFunctionStochasticCB-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3mbo::AcqFunction$eval_dt()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.html#method-eval_dt)
- [`mlr3mbo::AcqFunction$eval_many()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.html#method-eval_many)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqFunctionStochasticCB$new(
      surrogate = NULL,
      lambda = 1.96,
      min_lambda = 0.01,
      max_lambda = 10,
      distribution = "uniform",
      rate = 0,
      period = NULL
    )

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)).

- `lambda`:

  (`numeric(1)`).

- `min_lambda`:

  (`numeric(1)`).

- `max_lambda`:

  (`numeric(1)`).

- `distribution`:

  (`character(1)`).

- `rate`:

  (`numeric(1)`).

- `period`:

  (`NULL` \| `integer(1)`).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function. Samples and decays lambda.

#### Usage

    AcqFunctionStochasticCB$update()

------------------------------------------------------------------------

### Method `reset()`

Reset the acquisition function. Resets the private update counter `.t`
used within the epsilon decay.

#### Usage

    AcqFunctionStochasticCB$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionStochasticCB$clone(deep = FALSE)

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

  acq_function = acqf("stochastic_cb", surrogate = surrogate, lambda = 3)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>       acq_cb acq_lambda acq_lambda_0
#>        <num>      <num>        <num>
#> 1: -64.77109   3.346387     3.346387
#> 2: -64.67093   3.346387     3.346387
#> 3: -57.39852   3.346387     3.346387
```
