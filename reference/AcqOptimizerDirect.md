# Direct Acquisition Function Optimizer

Direct acquisition function optimizer. Calls `nloptr()` from
[nloptr](https://CRAN.R-project.org/package=nloptr). In its default
setting, the algorithm restarts `5 * D` times and runs at most for
`100 * D^2` function evaluations, where `D` is the dimension of the
search space. Each run stops when the relative tolerance of the
parameters is less than `10^-4`. The first iteration starts with the
best point in the archive and the next iterations start from a random
point.

## Note

If the restart strategy is `"none"`, the optimizer starts with the best
point in the archive. The optimization stops when one of the stopping
criteria is met.

If `restart_strategy` is `"random"`, the optimizer runs at least for
`maxeval` iterations. The first iteration starts with the best point in
the archive and stops when one of the stopping criteria is met. The next
iterations start from a random point.

## Parameters

- `restart_strategy`:

  `character(1)`  
  Restart strategy. Can be `"none"` or `"random"`. Default is `"none"`.

- `max_restarts`:

  `integer(1)`  
  Maximum number of restarts. Default is `5 * D` (Default).

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

[`mlr3mbo::AcqOptimizer`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
-\> `AcqOptimizerDirect`

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

## Methods

### Public methods

- [`AcqOptimizerDirect$new()`](#method-AcqOptimizerDirect-new)

- [`AcqOptimizerDirect$optimize()`](#method-AcqOptimizerDirect-optimize)

- [`AcqOptimizerDirect$clone()`](#method-AcqOptimizerDirect-clone)

Inherited methods

- [`mlr3mbo::AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-format)
- [`mlr3mbo::AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-print)
- [`mlr3mbo::AcqOptimizer$reset()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-reset)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerDirect$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Optimize the acquisition function.

#### Usage

    AcqOptimizerDirect$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### Method `clone()`

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
#> * Parameters: restart_strategy=random, catch_errors=TRUE
```
