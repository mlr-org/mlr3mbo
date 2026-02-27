# Random Search Acquisition Function Optimizer

Random search acquisition function optimizer. By default, it samples
`100 * D^2` random points in the search space, where `D` is the
dimension of the search space. The point with the highest acquisition
value is returned.

## Parameters

- `n_evals`:

  `integer(1)`  
  Number of random points to sample. Default is `100 * D^2`, where `D`
  is the dimension of the search space.

## Super class

[`mlr3mbo::AcqOptimizer`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
-\> `AcqOptimizerRandomSearch`

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

## Methods

### Public methods

- [`AcqOptimizerRandomSearch$new()`](#method-AcqOptimizerRandomSearch-new)

- [`AcqOptimizerRandomSearch$optimize()`](#method-AcqOptimizerRandomSearch-optimize)

- [`AcqOptimizerRandomSearch$clone()`](#method-AcqOptimizerRandomSearch-clone)

Inherited methods

- [`mlr3mbo::AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-format)
- [`mlr3mbo::AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-print)
- [`mlr3mbo::AcqOptimizer$reset()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-reset)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerRandomSearch$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Optimize the acquisition function.

#### Usage

    AcqOptimizerRandomSearch$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqOptimizerRandomSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
acqo("random_search")
#> <AcqOptimizerRandomSearch>: (OptimizerRandomSearch)
#> * Parameters: catch_errors=TRUE
```
