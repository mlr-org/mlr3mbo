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

- `skip_already_evaluated`:

  `logical(1)`  
  Should the proposed candidate be rejected if it was already evaluated
  on the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)?
  If `TRUE` and the candidate was already evaluated, an error is raised
  so that the `loop_function` can propose a randomly sampled point
  instead. Default is `TRUE`.

## Super class

[`AcqOptimizer`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
-\> `AcqOptimizerRandomSearch`

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

- [`AcqOptimizerRandomSearch$new()`](#method-AcqOptimizerRandomSearch-initialize)

- [`AcqOptimizerRandomSearch$optimize()`](#method-AcqOptimizerRandomSearch-optimize)

- [`AcqOptimizerRandomSearch$clone()`](#method-AcqOptimizerRandomSearch-clone)

Inherited methods

- [`AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-format)
- [`AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-print)
- [`AcqOptimizer$reset()`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.html#method-reset)

------------------------------------------------------------------------

### `AcqOptimizerRandomSearch$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerRandomSearch$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)).

------------------------------------------------------------------------

### `AcqOptimizerRandomSearch$optimize()`

Optimize the acquisition function.

#### Usage

    AcqOptimizerRandomSearch$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### `AcqOptimizerRandomSearch$clone()`

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
#> * Parameters: skip_already_evaluated=TRUE, catch_errors=TRUE
```
