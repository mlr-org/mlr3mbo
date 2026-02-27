# Acquisition Function Expected Improvement Per Second

Expected Improvement per Second.

It is assumed that calculations are performed on an
[bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html).
Additionally to target values of the codomain that should be minimized
or maximized, the
[bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html)
of the
[bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
should return time values. The column names of the target variable and
time variable must be passed as `cols_y` in the order `(target, time)`
when constructing the
[SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)
that is being used as a surrogate.

## Dictionary

This [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md):

    mlr_acqfunctions$get("eips")
    acqf("eips")

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
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md),
[`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei_log.md),
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
-\> `AcqFunctionEIPS`

## Public fields

- `y_best`:

  (`numeric(1)`)  
  Best objective function value observed so far. In the case of
  maximization, this already includes the necessary change of sign.

## Active bindings

- `col_y`:

  (`character(1)`).

- `col_time`:

  (`character(1)`).

## Methods

### Public methods

- [`AcqFunctionEIPS$new()`](#method-AcqFunctionEIPS-new)

- [`AcqFunctionEIPS$update()`](#method-AcqFunctionEIPS-update)

- [`AcqFunctionEIPS$clone()`](#method-AcqFunctionEIPS-clone)

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

    AcqFunctionEIPS$new(surrogate = NULL)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function and set `y_best`.

#### Usage

    AcqFunctionEIPS$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionEIPS$clone(deep = FALSE)

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
    list(y = xs$x ^ 2, time = abs(xs$x))
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))

  learner = default_gp()

  surrogate = srlrn(list(learner, learner$clone(deep = TRUE)), archive = instance$archive)
  surrogate$cols_y = c("y", "time")

  acq_function = acqf("eips", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>    acq_eips
#>       <num>
#> 1: 4.401238
#> 2: 4.864647
#> 3: 5.297136
```
