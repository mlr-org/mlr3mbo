# L-BFGS-B Acquisition Function Optimizer

L-BFGS-B acquisition function optimizer. Calls `nloptr()` from
[nloptr](https://CRAN.R-project.org/package=nloptr) with the
`NLOPT_LD_LBFGS` algorithm. In its default setting, the algorithm runs a
single time starting from the best point in the archive, for at most
`100 * D^2` function evaluations, where `D` is the dimension of the
search space. The run stops when the relative tolerance of the
parameters is less than `10^-4`. With `restart_strategy = "random"`, the
optimizer additionally restarts from random points until the evaluation
budget is exhausted, which can help escape local optima.

Only fully numeric search spaces (all parameters of type `p_dbl`) are
supported.

## Note

If the restart strategy is `"none"`, the optimizer runs a single time
starting from the best point in the archive. The optimization stops when
one of the stopping criteria is met.

If `restart_strategy` is `"random"`, the optimizer runs at most for
`maxeval` iterations in total. The first iteration starts with the best
point in the archive and stops when one of the stopping criteria is met.
The next iterations start from a random point.

## Parameters

- `restart_strategy`:

  `character(1)`  
  Restart strategy. Can be `"none"` or `"random"`. Default is `"none"`.

- `max_restarts`:

  `integer(1)`  
  Maximum number of restarts. Default is `5 * D`, where `D` is the
  dimension of the search space.

- `skip_already_evaluated`:

  `logical(1)`  
  Should the proposed candidate be rejected if it was already evaluated
  on the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)?
  If `TRUE` and the candidate was already evaluated, an error is raised
  so that the `loop_function` can propose a randomly sampled point
  instead. Default is `TRUE`.

## Termination Parameters

The following termination parameters can be used.

- `stopval`:

  `numeric(1)`  
  Stop value. Deactivate with `-Inf` (Default).

- `maxeval`:

  `integer(1)`  
  Maximum number of evaluations. Default is `100 * D^2`, where `D` is
  the dimension of the search space. Deactivate with `-1L`.

- `xtol_rel`:

  `numeric(1)`  
  Relative tolerance of the parameters. Default is `10^-4`. Deactivate
  with `-1`.

- `xtol_abs`:

  `numeric(1)`  
  Absolute tolerance of the parameters. Deactivate with `-1` (Default).

- `ftol_rel`:

  `numeric(1)`  
  Relative tolerance of the objective function. Deactivate with `-1`
  (Default).

- `ftol_abs`:

  `numeric(1)`  
  Absolute tolerance of the objective function. Deactivate with `-1`
  (Default).

## Super class

[`AcqOptimizer`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
-\> `AcqOptimizerLbfgsb`

## Public fields

- `state`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
  results.

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

## Methods

### Public methods

- [`AcqOptimizerLbfgsb$new()`](#method-AcqOptimizerLbfgsb-initialize)

- [`AcqOptimizerLbfgsb$optimize()`](#method-AcqOptimizerLbfgsb-optimize)

- [`AcqOptimizerLbfgsb$reset()`](#method-AcqOptimizerLbfgsb-reset)

- [`AcqOptimizerLbfgsb$clone()`](#method-AcqOptimizerLbfgsb-clone)

Inherited methods

- [`AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-format)
- [`AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-print)

------------------------------------------------------------------------

### `AcqOptimizerLbfgsb$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerLbfgsb$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)).

------------------------------------------------------------------------

### `AcqOptimizerLbfgsb$optimize()`

Optimize the acquisition function.

#### Usage

    AcqOptimizerLbfgsb$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### `AcqOptimizerLbfgsb$reset()`

Reset the acquisition function optimizer.

Clears the `state` of the previous optimization run.

#### Usage

    AcqOptimizerLbfgsb$reset()

------------------------------------------------------------------------

### `AcqOptimizerLbfgsb$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqOptimizerLbfgsb$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (requireNamespace("nloptr")) {
  acqo("lbfgsb")
}
#> <AcqOptimizerLbfgsb>: (OptimizerLbfgsb)
#> * Parameters: restart_strategy=none, skip_already_evaluated=TRUE,
#>   catch_errors=TRUE
```
