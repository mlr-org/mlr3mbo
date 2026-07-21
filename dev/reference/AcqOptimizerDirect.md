# Direct Acquisition Function Optimizer

Direct acquisition function optimizer. Calls `nloptr()` from
[nloptr](https://CRAN.R-project.org/package=nloptr) with the
`NLOPT_GN_DIRECT_L` algorithm. In its default setting, the algorithm
runs for at most `100 * D^2` function evaluations, where `D` is the
dimension of the search space. The optimization stops when the relative
tolerance of the parameters is less than `10^-4`.

Only fully numeric search spaces (all parameters of type `p_dbl`) are
supported.

## Note

`NLOPT_GN_DIRECT_L` is a deterministic global optimizer that ignores the
starting point. Restarts would only repeat the identical search, so the
optimizer does not support them.

## Parameters

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
  Relative tolerance of the objective function. Deactivate with `-1`.
  (Default).

- `ftol_abs`:

  `numeric(1)`  
  Absolute tolerance of the objective function. Deactivate with `-1`
  (Default).

## Super class

[`AcqOptimizer`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
-\> `AcqOptimizerDirect`

## Public fields

- `state`:

  ([`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
  result)  
  Result of the last optimization run.

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

- [`AcqOptimizerDirect$new()`](#method-AcqOptimizerDirect-initialize)

- [`AcqOptimizerDirect$optimize()`](#method-AcqOptimizerDirect-optimize)

- [`AcqOptimizerDirect$reset()`](#method-AcqOptimizerDirect-reset)

- [`AcqOptimizerDirect$clone()`](#method-AcqOptimizerDirect-clone)

Inherited methods

- [`AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-format)
- [`AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-print)

------------------------------------------------------------------------

### `AcqOptimizerDirect$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerDirect$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)).

------------------------------------------------------------------------

### `AcqOptimizerDirect$optimize()`

Optimize the acquisition function.

#### Usage

    AcqOptimizerDirect$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### `AcqOptimizerDirect$reset()`

Reset the acquisition function optimizer.

Clears the `state` of the previous optimization run.

#### Usage

    AcqOptimizerDirect$reset()

------------------------------------------------------------------------

### `AcqOptimizerDirect$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqOptimizerDirect$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (requireNamespace("nloptr")) {
  acqo("direct")
}
#> Loading required namespace: nloptr
#> <AcqOptimizerDirect>: (OptimizerDirect)
#> * Parameters: skip_already_evaluated=TRUE, catch_errors=TRUE
```
