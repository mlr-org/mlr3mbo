# Acquisition Function Expected Hypervolume Improvement

Exact Expected Hypervolume Improvement. Calculates the exact expected
hypervolume improvement in the case of two objectives. In the case of
optimizing more than two objective functions,
[AcqFunctionEHVIGH](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ehvigh.md)
can be used. See Emmerich et al. (2016) for details.

## References

- Emmerich, Michael, Yang, Kaifeng, Deutz, André, Wang, Hao, Fonseca, M.
  C (2016). “A Multicriteria Generalization of Bayesian Global
  Optimization.” In Pardalos, M. P, Zhigljavsky, Anatoly, Žilinskas,
  Julius (eds.), *Advances in Stochastic and Deterministic Global
  Optimization*, 229–242. Springer International Publishing, Cham.

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_cb.md),
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
-\> `AcqFunctionEHVI`

## Public fields

- `ys_front`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Approximated Pareto front. Sorted by the first objective. Signs are
  corrected with respect to assuming minimization of objectives.

- `ref_point`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Reference point. Signs are corrected with respect to assuming
  minimization of objectives.

- `ys_front_augmented`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Augmented approximated Pareto front. Sorted by the first objective.
  Signs are corrected with respect to assuming minimization of
  objectives.

## Methods

### Public methods

- [`AcqFunctionEHVI$new()`](#method-AcqFunctionEHVI-new)

- [`AcqFunctionEHVI$update()`](#method-AcqFunctionEHVI-update)

- [`AcqFunctionEHVI$clone()`](#method-AcqFunctionEHVI-clone)

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

    AcqFunctionEHVI$new(surrogate = NULL)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearnerCollection.md)).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function and set `ys_front` and `ref_point`.

#### Usage

    AcqFunctionEHVI$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionEHVI$clone(deep = FALSE)

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
    list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchMultiCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))

  learner = default_gp()

  surrogate = srlrn(list(learner, learner$clone(deep = TRUE)), archive = instance$archive)

  acq_function = acqf("ehvi", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>    acq_ehvi
#>       <num>
#> 1: 206.4270
#> 2: 264.3272
#> 3: 376.3496
```
