# Local Search Acquisition Function Optimizer

Local search acquisition function optimizer. Calls
[`bbotk::local_search()`](https://bbotk.mlr-org.com/reference/local_search.html).
For the meaning of the control parameters, see
[`bbotk::local_search_control()`](https://bbotk.mlr-org.com/reference/local_search_control.html).
The termination stops when the budget defined by the `n_searches`,
`n_steps`, and `n_neighs` parameters is exhausted.

If `skip_already_evaluated` is `TRUE` (default) and the proposed
candidate was already evaluated on the actual
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html),
an error is raised so that the `loop_function` can propose a randomly
sampled point instead.

## Super class

[`AcqOptimizer`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
-\> `AcqOptimizerLocalSearch`

## Public fields

- `state`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Result of the last
  [`bbotk::local_search()`](https://bbotk.mlr-org.com/reference/local_search.html)
  call.

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

- [`AcqOptimizerLocalSearch$new()`](#method-AcqOptimizerLocalSearch-initialize)

- [`AcqOptimizerLocalSearch$optimize()`](#method-AcqOptimizerLocalSearch-optimize)

- [`AcqOptimizerLocalSearch$reset()`](#method-AcqOptimizerLocalSearch-reset)

- [`AcqOptimizerLocalSearch$clone()`](#method-AcqOptimizerLocalSearch-clone)

Inherited methods

- [`AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-format)
- [`AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-print)

------------------------------------------------------------------------

### `AcqOptimizerLocalSearch$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerLocalSearch$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

------------------------------------------------------------------------

### `AcqOptimizerLocalSearch$optimize()`

Optimize the acquisition function.

#### Usage

    AcqOptimizerLocalSearch$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### `AcqOptimizerLocalSearch$reset()`

Reset the acquisition function optimizer.

Clears the `state` of the previous optimization run.

#### Usage

    AcqOptimizerLocalSearch$reset()

------------------------------------------------------------------------

### `AcqOptimizerLocalSearch$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqOptimizerLocalSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
acqo("local_search")
#> <AcqOptimizerLocalSearch>: (OptimizerLocalSearch)
#> * Parameters: skip_already_evaluated=TRUE, catch_errors=TRUE
```
