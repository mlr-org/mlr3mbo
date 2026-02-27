# Acquisition Function Wrapping Multiple Acquisition Functions

Wrapping multiple
[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)s
resulting in a multi-objective acquisition function composed of the
individual ones. Note that the optimization direction of each wrapped
acquisition function is corrected for maximization.

For each acquisition function, the same
[Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md) must be
used. If acquisition functions passed during construction already have
been initialized with a surrogate, it is checked whether the surrogate
is the same for all acquisition functions. If acquisition functions have
not been initialized with a surrogate, the surrogate passed during
construction or lazy initialization will be used for all acquisition
functions.

For optimization,
[AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
can be used as for any other
[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md),
however, the
[bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
wrapped within the
[AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
must support multi-objective optimization as indicated via the
`multi-crit` property.

## Dictionary

This [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
or with the associated sugar function
[`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md):

    mlr_acqfunctions$get("multi")
    acqf("multi")

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
[`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_pi.md),
[`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.md),
[`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md),
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_ei.md)

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3mbo::AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
-\> `AcqFunctionMulti`

## Active bindings

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md))  
  Surrogate.

- `acq_functions`:

  (list of
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md))  
  Points to the list of the individual acquisition functions.

- `acq_function_ids`:

  (character())  
  Points to the ids of the individual acquisition functions.

## Methods

### Public methods

- [`AcqFunctionMulti$new()`](#method-AcqFunctionMulti-new)

- [`AcqFunctionMulti$update()`](#method-AcqFunctionMulti-update)

- [`AcqFunctionMulti$clone()`](#method-AcqFunctionMulti-clone)

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

    AcqFunctionMulti$new(acq_functions, surrogate = NULL)

#### Arguments

- `acq_functions`:

  (list of
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)s).

- `surrogate`:

  (`NULL` \|
  [Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md)).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update each of the wrapped acquisition functions.

#### Usage

    AcqFunctionMulti$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionMulti$clone(deep = FALSE)

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

  acq_function = acqf("multi",
    acq_functions = acqfs(c("ei", "pi", "cb")),
    surrogate = surrogate
  )

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>      acq_ei    acq_pi   acq_cb
#>       <num>     <num>    <num>
#> 1: 4.401492 0.2666944 28.30303
#> 2: 4.864906 0.2939694 29.30439
#> 3: 5.297345 0.3509548 27.23401
```
