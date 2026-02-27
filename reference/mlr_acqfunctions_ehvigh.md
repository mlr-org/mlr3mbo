# Acquisition Function Expected Hypervolume Improvement via Gauss-Hermite Quadrature

Expected Hypervolume Improvement. Computed via Gauss-Hermite quadrature.

In the case of optimizing only two objective functions
[AcqFunctionEHVI](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md)
is to be preferred.

## Parameters

- `"k"` (`integer(1)`)  
  Number of nodes per objective used for the numerical integration via
  Gauss-Hermite quadrature. Defaults to `15`. For example, if two
  objectives are to be optimized, the total number of nodes will
  therefore be 225 per default. Changing this value after construction
  requires a call to `$update()` to update the `$gh_data` field.

- `"r"` (`numeric(1)`)  
  Pruning rate between 0 and 1 that determines the fraction of nodes of
  the Gauss-Hermite quadrature rule that are ignored based on their
  weight value (the nodes with the lowest weights being ignored).
  Default is `0.2`. Changing this value after construction does not
  require a call to `$update()`.

## References

- Rahat, Alma, Chugh, Tinkle, Fieldsend, Jonathan, Allmendinger,
  Richard, Miettinen, Kaisa (2022). “Efficient Approximation of Expected
  Hypervolume Improvement using Gauss-Hermit Quadrature.” In Rudolph,
  Günter, Kononova, V. A, Aguirre, Hernán, Kerschke, Pascal, Ochoa,
  Gabriela, Tušar, Tea (eds.), *Parallel Problem Solving from Nature –
  PPSN XVII*, 90–103.

## See also

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md),
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md),
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
-\> `AcqFunctionEHVIGH`

## Public fields

- `ys_front`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Approximated Pareto front. Signs are corrected with respect to
  assuming minimization of objectives.

- `ref_point`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Reference point. Signs are corrected with respect to assuming
  minimization of objectives.

- `hypervolume`:

  (`numeric(1)`). Current hypervolume of the approximated Pareto front
  with respect to the reference point.

- `gh_data`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Data required for the Gauss-Hermite quadrature rule in the form of a
  matrix of dimension (k x 2). Each row corresponds to one Gauss-Hermite
  node (column `"x"`) and corresponding weight (column `"w"`). Computed
  via
  [fastGHQuad::gaussHermiteData](https://rdrr.io/pkg/fastGHQuad/man/gaussHermiteData.html).
  Nodes are scaled by a factor of `sqrt(2)` and weights are normalized
  under a sum to one constraint.

## Methods

### Public methods

- [`AcqFunctionEHVIGH$new()`](#method-AcqFunctionEHVIGH-new)

- [`AcqFunctionEHVIGH$update()`](#method-AcqFunctionEHVIGH-update)

- [`AcqFunctionEHVIGH$clone()`](#method-AcqFunctionEHVIGH-clone)

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

    AcqFunctionEHVIGH$new(surrogate = NULL, k = 15L, r = 0.2)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)).

- `k`:

  (`integer(1)`).

- `r`:

  (`numeric(1)`).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function and set `ys_front`, `ref_point`,
`hypervolume` and `gh_data`.

#### Usage

    AcqFunctionEHVIGH$update()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionEHVIGH$clone(deep = FALSE)

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

  acq_function = acqf("ehvigh", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>    acq_ehvigh
#>         <num>
#> 1:   215.1581
#> 2:   276.4326
#> 3:   374.5412
```
