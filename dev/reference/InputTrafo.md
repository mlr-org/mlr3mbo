# Input Transformation Base Class

Abstract input transformation class.

An input transformation can be used within a
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) to
perform a transformation of the feature variables.

## See also

Other Input Transformation:
[`InputTrafoUnitcube`](https://mlr3mbo.mlr-org.com/dev/reference/InputTrafoUnitcube.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_input_trafos.md)

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

- `state`:

  (named [`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
  List of meta information regarding the parameters and their state.

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space.

- `cols_x`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Column ids of feature variables that should be transformed.

## Methods

### Public methods

- [`InputTrafo$new()`](#method-InputTrafo-new)

- [`InputTrafo$update()`](#method-InputTrafo-update)

- [`InputTrafo$transform()`](#method-InputTrafo-transform)

- [`InputTrafo$format()`](#method-InputTrafo-format)

- [`InputTrafo$print()`](#method-InputTrafo-print)

- [`InputTrafo$clone()`](#method-InputTrafo-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    InputTrafo$new(label = NA_character_, man = NA_character_)

#### Arguments

- `label`:

  (`character(1)`)  
  Label for this object.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Learn the transformation based on observed data and update parameters in
`$state`. Must be implemented by subclasses.

#### Usage

    InputTrafo$update(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_x`.

------------------------------------------------------------------------

### Method [`transform()`](https://rdrr.io/r/base/transform.html)

Perform the transformation. Must be implemented by subclasses.

#### Usage

    InputTrafo$transform(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_x`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the transformation applied to the columns `$cols_x` (if applicable)
or a subset thereof.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    InputTrafo$format()

#### Returns

(`character(1)`).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    InputTrafo$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    InputTrafo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
