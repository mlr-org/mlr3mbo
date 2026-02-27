# Result Assigner Base Class

Abstract result assigner class.

A result assigner is responsible for assigning the final optimization
result to the
[bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).
Normally, it is only used within an
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).

## See also

Other Result Assigner:
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners.md),
[`mlr_result_assigners_archive`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_archive.md),
[`mlr_result_assigners_surrogate`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_surrogate.md)

## Active bindings

- `label`:

  (`character(1)`)  
  Label for this object.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled if at least one of the
  packages is not installed, but loaded (not attached) later on-demand
  via [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

## Methods

### Public methods

- [`ResultAssigner$new()`](#method-ResultAssigner-new)

- [`ResultAssigner$assign_result()`](#method-ResultAssigner-assign_result)

- [`ResultAssigner$format()`](#method-ResultAssigner-format)

- [`ResultAssigner$print()`](#method-ResultAssigner-print)

- [`ResultAssigner$clone()`](#method-ResultAssigner-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResultAssigner$new(label = NA_character_, man = NA_character_)

#### Arguments

- `label`:

  (`character(1)`)  
  Label for this object.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

------------------------------------------------------------------------

### Method `assign_result()`

Assigns the result, i.e., the final point(s) to the instance.

#### Usage

    ResultAssigner$assign_result(instance)

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

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    ResultAssigner$format()

#### Returns

(`character(1)`).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    ResultAssigner$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResultAssigner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
