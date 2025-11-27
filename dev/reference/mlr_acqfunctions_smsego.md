# Acquisition Function SMS-EGO

S-Metric Selection Evolutionary Multi-Objective Optimization Algorithm
Acquisition Function.

## Parameters

- `"lambda"` (`numeric(1)`)  
  \\\lambda\\ value used for the confidence bound. Defaults to `1`.
  Based on `confidence = (1 - 2 * dnorm(lambda)) ^ m` you can calculate
  a lambda for a given confidence level, see Ponweiser et al. (2008).

- `"epsilon"` (`numeric(1)`)  
  \\\epsilon\\ used for the additive epsilon dominance. Can either be a
  single numeric value \> 0 or `NULL` (default). In the case of being
  `NULL`, an epsilon vector is maintained dynamically as described in
  Horn et al. (2015).

## Note

- This acquisition function always also returns its current epsilon
  values in a list column (`acq_epsilon`). These values will be logged
  into the
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

- Ponweiser, Wolfgang, Wagner, Tobias, Biermann, Dirk, Vincze, Markus
  (2008). “Multiobjective Optimization on a Limited Budget of
  Evaluations Using Model-Assisted S-Metric Selection.” In *Proceedings
  of the 10th International Conference on Parallel Problem Solving from
  Nature*, 784–794.

- Horn, Daniel, Wagner, Tobias, Biermann, Dirk, Weihs, Claus, Bischl,
  Bernd (2015). “Model-Based Multi-objective Optimization: Taxonomy,
  Multi-Point Proposal, Toolbox and Benchmark.” In *International
  Conference on Evolutionary Multi-Criterion Optimization*, 64–78.

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
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_ei.md)

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3mbo::AcqFunction`](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
-\> `AcqFunctionSmsEgo`

## Public fields

- `ys_front`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Approximated Pareto front. Signs are corrected with respect to
  assuming minimization of objectives.

- `ref_point`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Reference point. Signs are corrected with respect to assuming
  minimization of objectives.

- `epsilon`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Epsilon used for the additive epsilon dominance.

- `progress`:

  (`numeric(1)`)  
  Optimization progress (typically, the number of function evaluations
  left). Note that this requires the
  [bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
  to be terminated via a
  [bbotk::TerminatorEvals](https://bbotk.mlr-org.com/reference/mlr_terminators_evals.html).

## Methods

### Public methods

- [`AcqFunctionSmsEgo$new()`](#method-AcqFunctionSmsEgo-new)

- [`AcqFunctionSmsEgo$update()`](#method-AcqFunctionSmsEgo-update)

- [`AcqFunctionSmsEgo$reset()`](#method-AcqFunctionSmsEgo-reset)

- [`AcqFunctionSmsEgo$clone()`](#method-AcqFunctionSmsEgo-clone)

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

    AcqFunctionSmsEgo$new(surrogate = NULL, lambda = 1, epsilon = NULL)

#### Arguments

- `surrogate`:

  (`NULL` \|
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearnerCollection.md)).

- `lambda`:

  (`numeric(1)`).

- `epsilon`:

  (`NULL` \| `numeric(1)`).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function and set `ys_front`, `ref_point` and
`epsilon`.

#### Usage

    AcqFunctionSmsEgo$update()

------------------------------------------------------------------------

### Method `reset()`

Reset the acquisition function. Resets `epsilon`.

#### Usage

    AcqFunctionSmsEgo$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunctionSmsEgo$clone(deep = FALSE)

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

  acq_function = acqf("smsego", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$progress = 5 - 4 # n_evals = 5 and 4 points already evaluated
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
#>    acq_smsego acq_epsilon
#>         <num>      <list>
#> 1:  -581.3276         0,0
#> 2:  -730.5624         0,0
#> 3: -1132.3142         0,0
```
