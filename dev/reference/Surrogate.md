# Surrogate Model

Abstract surrogate model class.

A surrogate model is used to model the unknown objective function(s)
based on all points evaluated so far.

## Public fields

- `learner`:

  (learner)  
  Arbitrary learner object depending on the subclass.

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

- `archive`:

  ([bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) \|
  `NULL`)  
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

- `archive_is_async`:

  (\`bool(1)“)  
  Whether the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) is
  an asynchronous one.

- `n_learner`:

  (`integer(1)`)  
  Returns the number of surrogate models.

- `cols_x`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column ids of variables that should be used as features. By default,
  automatically inferred based on the archive.

- `cols_y`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column ids of variables that should be used as targets. By default,
  automatically inferred based on the archive.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled if at least one of the
  packages is not installed, but loaded (not attached) later on-demand
  via [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores the feature types the surrogate can handle, e.g. `"logical"`,
  `"numeric"`, or `"factor"`. A complete list of candidate feature
  types, grouped by task type, is stored in
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores a set of properties/capabilities the surrogate has. A complete
  list of candidate properties, grouped by task type, is stored in
  [`mlr_reflections$learner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `predict_type`:

  (`character(1)`)  
  Retrieves the currently active predict type, e.g. `"response"`.

## Methods

### Public methods

- [`Surrogate$new()`](#method-Surrogate-new)

- [`Surrogate$update()`](#method-Surrogate-update)

- [`Surrogate$reset()`](#method-Surrogate-reset)

- [`Surrogate$predict()`](#method-Surrogate-predict)

- [`Surrogate$format()`](#method-Surrogate-format)

- [`Surrogate$print()`](#method-Surrogate-print)

- [`Surrogate$clone()`](#method-Surrogate-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Surrogate$new(learner, archive, cols_x, cols_y, param_set)

#### Arguments

- `learner`:

  (learner)  
  Arbitrary learner object depending on the subclass.

- `archive`:

  ([bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) \|
  `NULL`)  
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

- `cols_x`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column id's of variables that should be used as features. By default,
  automatically inferred based on the archive.

- `cols_y`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column id's of variables that should be used as targets. By default,
  automatically inferred based on the archive.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter space description depending on the subclass.

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Train learner with new data. Subclasses must implement
`private.update()` and `private.update_async()`.

#### Usage

    Surrogate$update()

#### Returns

`NULL`.

------------------------------------------------------------------------

### Method `reset()`

Reset the surrogate model. Subclasses must implement `private$.reset()`.

#### Usage

    Surrogate$reset()

#### Returns

`NULL`

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Predict mean response and standard error. Must be implemented by
subclasses.

#### Usage

    Surrogate$predict(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  New data. One row per observation.

#### Returns

Arbitrary prediction object.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Surrogate$format()

#### Returns

(`character(1)`).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    Surrogate$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Surrogate$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
