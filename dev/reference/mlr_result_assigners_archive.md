# Result Assigner Based on the Archive

Result assigner that chooses the final point(s) based on all evaluations
in the
[bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html). This
mimics the default behavior of any
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html).

## See also

Other Result Assigner:
[`ResultAssigner`](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md),
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners.md),
[`mlr_result_assigners_surrogate`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners_surrogate.md)

## Super class

[`mlr3mbo::ResultAssigner`](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md)
-\> `ResultAssignerArchive`

## Active bindings

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled if at least one of the
  packages is not installed, but loaded (not attached) later on-demand
  via [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

## Methods

### Public methods

- [`ResultAssignerArchive$new()`](#method-ResultAssignerArchive-new)

- [`ResultAssignerArchive$assign_result()`](#method-ResultAssignerArchive-assign_result)

- [`ResultAssignerArchive$clone()`](#method-ResultAssignerArchive-clone)

Inherited methods

- [`mlr3mbo::ResultAssigner$format()`](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.html#method-format)
- [`mlr3mbo::ResultAssigner$print()`](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResultAssignerArchive$new()

------------------------------------------------------------------------

### Method `assign_result()`

Assigns the result, i.e., the final point(s) to the instance.

#### Usage

    ResultAssignerArchive$assign_result(instance)

#### Arguments

- `instance`:

  ([bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
  \|
  [bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html)
  \|[bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
  \|
  [bbotk::OptimInstanceAsyncMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncMultiCrit.html))  
  The
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  the final result should be assigned to.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResultAssignerArchive$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
result_assigner = ras("archive")
```
