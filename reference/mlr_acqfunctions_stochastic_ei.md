# Acquisition Function Stochastic Expected Improvement

Expected Improvement with epsilon decay. \\\epsilon\\ is updated after
each update by the formula `epsilon * exp(-rate * (t %% period))` where
`t` is the number of times the acquisition function has been updated.

While this acquisition function usually would be used within an
asynchronous optimizer, e.g.,
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md),
it can in principle also be used in synchronous optimizers, e.g.,
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).

## Dictionary

This [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md):

    mlr_acqfunctions$get("stochastic_ei")
    acqf("stochastic_ei")

## Parameters

- `"epsilon"` (`numeric(1)`)  
  \\\epsilon\\ value used to determine the amount of exploration. Higher
  values result in the importance of improvements predicted by the
  posterior mean decreasing relative to the importance of potential
  improvements in regions of high predictive uncertainty. Defaults to
  `0.1`.

- `"rate"` (`numeric(1)`)  
  Defaults to `0.05`.

- `"period"` (`integer(1)`)  
  Period of the exponential decay. Defaults to `NULL`, i.e., the decay
  has no period.

## Note

- This acquisition function always also returns its current
  (`acq_epsilon`) and original (`acq_epsilon_0`) \\\epsilon\\. These
  values will be logged into the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  of the
  [bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
  of the
  [AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
  and therefore also in the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  that is to be optimized.

## References

- Jones, R. D, Schonlau, Matthias, Welch, J. W (1998). “Efficient Global
  Optimization of Expensive Black-Box Functions.” *Journal of Global
  optimization*, **13**(4), 455–492.

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md),
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
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md)

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3mbo::AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
-\> `AcqFunctionStochasticEI`

## Public fields

- `y_best`:

  (`numeric(1)`)  
  Best objective function value observed so far. In the case of
  maximization, this already includes the necessary change of sign.

## Methods

### Public methods

- [`AcqFunctionStochasticEI$new()`](#method-AcqFunctionStochasticEI-new)

- [`AcqFunctionStochasticEI$update()`](#method-AcqFunctionStochasticEI-update)

- [`AcqFunctionStochasticEI$reset()`](#method-AcqFunctionStochasticEI-reset)

- [`AcqFunctionStochasticEI$clone()`](#method-AcqFunctionStochasticEI-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3mbo::AcqFunction$eval_dt()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-eval_dt)
- [`mlr3mbo::AcqFunction$eval_many()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-eval_many)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqFunctionStochasticEI$new(
      surrogate = NULL,
      epsilon = 0.1,
      rate = 0.05,
      period = NULL
    )

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)).

- `epsilon`:

  (`numeric(1)`).

- `rate`:

  (`numeric(1)`).

- `period`:

  (`NULL` \| `integer(1)`).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function. Sets `y_best` to the best observed
objective function value. Decays epsilon.

#### Usage

    AcqFunctionStochasticEI$update()

------------------------------------------------------------------------

### Method `reset()`

Reset the acquisition function. Resets the private update counter `.t`
used within the epsilon decay.

#### Usage

    AcqFunctionStochasticEI$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionStochasticEI$clone(deep = FALSE)

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

  acq_function = acqf("stochastic_ei", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>      acq_ei acq_epsilon acq_epsilon_0
#>       <num>       <num>         <num>
#> 1: 4.374250         0.1           0.1
#> 2: 4.834928         0.1           0.1
#> 3: 5.261825         0.1           0.1
```
