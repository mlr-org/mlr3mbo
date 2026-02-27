# Acquisition Function Optimizer

Optimizer for
[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)s
which performs the acquisition function optimization. Wraps an
[bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
and
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).

## Parameters

- `n_candidates`:

  `integer(1)`  
  Number of candidate points to propose. Note that this does not affect
  how the acquisition function itself is calculated (e.g., setting
  `n_candidates > 1` will not result in computing the q- or
  multi-Expected Improvement) but rather the top `n_candidates` are
  selected from the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  of the acquisition function
  [bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html).
  Note that setting `n_candidates > 1` is usually not a sensible idea
  but it is still supported for experimental reasons. Note that in the
  case of the acquisition function
  [bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
  being multi-objective, due to using an
  [AcqFunctionMulti](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_multi.md),
  selection of the best candidates is performed via
  non-dominated-sorting. Default is `1`.

- `logging_level`:

  `character(1)`  
  Logging level during the acquisition function optimization. Can be
  `"fatal"`, `"error"`, `"warn"`, `"info"`, `"debug"` or `"trace"`.
  Default is `"warn"`, i.e., only warnings are logged.

- `warmstart`:

  `logical(1)`  
  Should the acquisition function optimization be warm-started by
  evaluating the best point(s) present in the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  (which is contained in the archive of the
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md))?
  This is sensible when using a population based acquisition function
  optimizer, e.g., local search or mutation. Default is `FALSE`. Note
  that in the case of the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  being multi-objective, selection of the best point(s) is performed via
  non-dominated-sorting.

- `warmstart_size`:

  `integer(1) | "all"`  
  Number of best points selected from the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  that are to be used for warm starting. Can either be an integer or
  "all" to use all available points. Only relevant if
  `warmstart = TRUE`. Default is `1`.

- `skip_already_evaluated`:

  `logical(1)`  
  It can happen that the candidate(s) resulting of the acquisition
  function optimization were already evaluated on the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).
  Should such candidate proposals be ignored and only candidates that
  were yet not evaluated be considered? Default is `TRUE`.

- `catch_errors`:

  `logical(1)`  
  Should errors during the acquisition function optimization be caught
  and propagated to the `loop_function` which can then handle the failed
  acquisition function optimization appropriately by, e.g., proposing a
  randomly sampled point for evaluation? Setting this to `FALSE` can be
  helpful for debugging. Default is `TRUE`.

## Public fields

- `optimizer`:

  ([bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)).

- `terminator`:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)).

- `acq_function`:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

- `callbacks`:

  (`NULL` \| list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)).

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

## Methods

### Public methods

- [`AcqOptimizer$new()`](#method-AcqOptimizer-new)

- [`AcqOptimizer$format()`](#method-AcqOptimizer-format)

- [`AcqOptimizer$print()`](#method-AcqOptimizer-print)

- [`AcqOptimizer$optimize()`](#method-AcqOptimizer-optimize)

- [`AcqOptimizer$reset()`](#method-AcqOptimizer-reset)

- [`AcqOptimizer$clone()`](#method-AcqOptimizer-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizer$new(optimizer, terminator, acq_function = NULL, callbacks = NULL)

#### Arguments

- `optimizer`:

  ([bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)).

- `terminator`:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)).

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

- `callbacks`:

  (`NULL` \| list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    AcqOptimizer$format()

#### Returns

(`character(1)`).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    AcqOptimizer$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Optimize the acquisition function.

#### Usage

    AcqOptimizer$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### Method `reset()`

Reset the acquisition function optimizer.

Currently not used.

#### Usage

    AcqOptimizer$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqOptimizer$clone(deep = FALSE)

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

  acq_function = acqf("ei", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()

  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 1000),
    terminator = trm("evals", n_evals = 1000),
    acq_function = acq_function)

  acq_optimizer$optimize()
}
#> Loading required namespace: DiceKriging
#> Loading required namespace: rgenoud
#>           x   acq_ei  x_domain .already_evaluated
#>       <num>    <num>    <list>             <lgcl>
#> 1: 1.187665 5.305187 <list[1]>              FALSE
```
