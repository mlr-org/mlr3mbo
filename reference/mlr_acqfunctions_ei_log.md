# Acquisition Function Expected Improvement on Log Scale

Expected Improvement assuming that the target variable has been modeled
on log scale. In general only sensible if the
[SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
uses an
[OutputTrafoLog](https://mlr3mbo.mlr-org.com/reference/OutputTrafoLog.md)
without inverting the posterior predictive distribution
(`invert_posterior = FALSE`). See also the example below.

## Dictionary

This [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md):

    mlr_acqfunctions$get("ei_log")
    acqf("ei_log")

## Parameters

- `"epsilon"` (`numeric(1)`)  
  \\\epsilon\\ value used to determine the amount of exploration. Higher
  values result in the importance of improvements predicted by the
  posterior mean decreasing relative to the importance of potential
  improvements in regions of high predictive uncertainty. Defaults to
  `0` (standard Expected Improvement).

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md),
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
-\> `AcqFunctionEILog`

## Public fields

- `y_best`:

  (`numeric(1)`)  
  Best objective function value observed so far. In the case of
  maximization, this already includes the necessary change of sign.

## Methods

### Public methods

- [`AcqFunctionEILog$new()`](#method-AcqFunctionEILog-new)

- [`AcqFunctionEILog$update()`](#method-AcqFunctionEILog-update)

- [`AcqFunctionEILog$clone()`](#method-AcqFunctionEILog-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3mbo::AcqFunction$eval_dt()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-eval_dt)
- [`mlr3mbo::AcqFunction$eval_many()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-eval_many)
- [`mlr3mbo::AcqFunction$reset()`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html#method-reset)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqFunctionEILog$new(surrogate = NULL, epsilon = 0)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)).

- `epsilon`:

  (`numeric(1)`).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function and set `y_best`.

#### Usage

    AcqFunctionEILog$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionEILog$clone(deep = FALSE)

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

  output_trafo = ot("log", invert_posterior = FALSE)

  surrogate = srlrn(learner, output_trafo = output_trafo, archive = instance$archive)

  acq_function = acqf("ei_log", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>     acq_ei_log
#>          <num>
#> 1: 0.004804734
#> 2: 0.008287281
#> 3: 0.012162085
```
