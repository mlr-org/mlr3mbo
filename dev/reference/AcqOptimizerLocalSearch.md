# Local Search Acquisition Function Optimizer

Local search acquisition function optimizer. Calls
[`bbotk::local_search()`](https://bbotk.mlr-org.com/reference/local_search.html).
For the meaning of the control parameters, see
[`bbotk::local_search_control()`](https://bbotk.mlr-org.com/reference/local_search_control.html).
The termination stops when the budget defined by the `n_searches`,
`n_steps`, and `n_neighs` parameters is exhausted.

## Super class

[`mlr3mbo::AcqOptimizer`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
-\> `AcqOptimizerLocalSearch`

## Public fields

- `state`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of `cmaes::cma_es()` results.

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

## Methods

### Public methods

- [`AcqOptimizerLocalSearch$new()`](#method-AcqOptimizerLocalSearch-new)

- [`AcqOptimizerLocalSearch$optimize()`](#method-AcqOptimizerLocalSearch-optimize)

- [`AcqOptimizerLocalSearch$reset()`](#method-AcqOptimizerLocalSearch-reset)

- [`AcqOptimizerLocalSearch$clone()`](#method-AcqOptimizerLocalSearch-clone)

Inherited methods

- [`mlr3mbo::AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-format)
- [`mlr3mbo::AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerLocalSearch$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)).

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Optimize the acquisition function.

#### Usage

    AcqOptimizerLocalSearch$optimize()

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### Method `reset()`

Reset the acquisition function optimizer.

Currently not used.

#### Usage

    AcqOptimizerLocalSearch$reset()

------------------------------------------------------------------------

### Method `clone()`

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
#> * Parameters: catch_errors=TRUE
```
