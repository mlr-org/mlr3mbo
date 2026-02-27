# Output Transformation Base Class

Abstract output transformation class.

An output transformation can be used within a
[Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md) to
perform a transformation of the target variable(s).

## See also

Other Output Transformation:
[`OutputTrafoLog`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoLog.md),
[`OutputTrafoStandardize`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoStandardize.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_output_trafos.md)

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

- `cols_y`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column ids of target variables that should be transformed.

- `max_to_min`:

  (`-1` \| `1`)  
  Multiplicative factor to correct for minimization or maximization.

- `invert_posterior`:

  (`logical(1)`)  
  Should the posterior predictive distribution be inverted when used
  within a
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
  or
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)?

## Methods

### Public methods

- [`OutputTrafo$new()`](#method-OutputTrafo-new)

- [`OutputTrafo$update()`](#method-OutputTrafo-update)

- [`OutputTrafo$transform()`](#method-OutputTrafo-transform)

- [`OutputTrafo$inverse_transform_posterior()`](#method-OutputTrafo-inverse_transform_posterior)

- [`OutputTrafo$inverse_transform()`](#method-OutputTrafo-inverse_transform)

- [`OutputTrafo$format()`](#method-OutputTrafo-format)

- [`OutputTrafo$print()`](#method-OutputTrafo-print)

- [`OutputTrafo$clone()`](#method-OutputTrafo-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OutputTrafo$new(invert_posterior, label = NA_character_, man = NA_character_)

#### Arguments

- `invert_posterior`:

  (`logical(1)`)  
  Should the posterior predictive distribution be inverted when used
  within a
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
  or
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)?

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

    OutputTrafo$update(ydt)

#### Arguments

- `ydt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_y`.

------------------------------------------------------------------------

### Method [`transform()`](https://rdrr.io/r/base/transform.html)

Perform the transformation. Must be implemented by subclasses.

#### Usage

    OutputTrafo$transform(ydt)

#### Arguments

- `ydt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_y`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the transformation applied to the columns `$cols_y`.

------------------------------------------------------------------------

### Method `inverse_transform_posterior()`

Perform the inverse transformation on a posterior predictive
distribution characterized by the first and second moment. Must be
implemented by subclasses.

#### Usage

    OutputTrafo$inverse_transform_posterior(pred)

#### Arguments

- `pred`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation characterizing a posterior predictive
  distribution with the columns `mean` and `se`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the inverse transformation applied to the columns `mean` and `se`.

------------------------------------------------------------------------

### Method `inverse_transform()`

Perform the inverse transformation. Must be implemented by subclasses.

#### Usage

    OutputTrafo$inverse_transform(ydt)

#### Arguments

- `ydt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_y`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the inverse transformation applied to the columns `$cols_y`.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    OutputTrafo$format()

#### Returns

(`character(1)`).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    OutputTrafo$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputTrafo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
