# Acquisition Function Base Class

Abstract acquisition function class.

Based on the predictions of a
[Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md), the
acquisition function encodes the preference to evaluate a new point.

## See also

Other Acquisition Function:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md),
[`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei_log.md),
[`mlr_acqfunctions_eips`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_eips.md),
[`mlr_acqfunctions_mean`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_mean.md),
[`mlr_acqfunctions_multi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_multi.md),
[`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_pi.md),
[`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.md),
[`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md),
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_ei.md)

## Super class

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\> `AcqFunction`

## Active bindings

- `direction`:

  (`"same"` \| `"minimize"` \| `"maximize"`)  
  Optimization direction of the acquisition function relative to the
  direction of the objective function of the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  related to the passed
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html).
  Must be `"same"`, `"minimize"`, or `"maximize"`.

- `surrogate_max_to_min`:

  (`-1` \| `1`)  
  Multiplicative factor to correct for minimization or maximization of
  the acquisition function.

- `label`:

  (`character(1)`)  
  Label for this object.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

- `archive`:

  ([bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html))  
  Points to the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the surrogate.

- `fun`:

  (`function`)  
  Points to the private acquisition function to be implemented by
  subclasses.

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md))  
  Surrogate.

- `requires_predict_type_se`:

  (`logical(1)`)  
  Whether the acquisition function requires the surrogate to have `"se"`
  as `$predict_type`.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages.

## Methods

### Public methods

- [`AcqFunction$new()`](#method-AcqFunction-new)

- [`AcqFunction$update()`](#method-AcqFunction-update)

- [`AcqFunction$reset()`](#method-AcqFunction-reset)

- [`AcqFunction$eval_many()`](#method-AcqFunction-eval_many)

- [`AcqFunction$eval_dt()`](#method-AcqFunction-eval_dt)

- [`AcqFunction$clone()`](#method-AcqFunction-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that the surrogate can be initialized lazy and can later be set via
the active binding `$surrogate`.

#### Usage

    AcqFunction$new(
      id,
      constants = ParamSet$new(),
      surrogate,
      requires_predict_type_se,
      direction,
      packages = NULL,
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`).

- `constants`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)).
  Changeable constants or parameters.

- `surrogate`:

  (`NULL` \|
  [Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md)).
  Surrogate whose predictions are used in the acquisition function.

- `requires_predict_type_se`:

  (`logical(1)`)  
  Whether the acquisition function requires the surrogate to have `"se"`
  as `$predict_type`.

- `direction`:

  (`"same"` \| `"minimize"` \| `"maximize"`). Optimization direction of
  the acquisition function relative to the direction of the objective
  function of the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).
  Must be `"same"`, `"minimize"`, or `"maximize"`.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled prior to construction
  if at least one of the packages is not installed, but loaded (not
  attached) later on-demand via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

- `label`:

  (`character(1)`)  
  Label for this object.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the acquisition function.

Can be implemented by subclasses.

#### Usage

    AcqFunction$update()

------------------------------------------------------------------------

### Method `reset()`

Reset the acquisition function.

Can be implemented by subclasses.

#### Usage

    AcqFunction$reset()

------------------------------------------------------------------------

### Method `eval_many()`

Evaluates multiple input values on the acquisition function.

#### Usage

    AcqFunction$eval_many(xss)

#### Arguments

- `xss`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  A list of lists that contains multiple x values, e.g.
  `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.

#### Returns

data.table::data.table() that contains one y-column for single-objective
acquisition functions and multiple y-columns for multi-objective
acquisition functions, e.g. `data.table(y = 1:2)` or
`data.table(y1 = 1:2, y2 = 3:4)`.

------------------------------------------------------------------------

### Method `eval_dt()`

Evaluates multiple input values on the objective function

#### Usage

    AcqFunction$eval_dt(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  One point per row, e.g. `data.table(x1 = c(1, 3), x2 = c(2, 4))`.

#### Returns

data.table::data.table() that contains one y-column for single-objective
acquisition functions and multiple y-columns for multi-objective
acquisition functions, e.g. `data.table(y = 1:2)` or
`data.table(y1 = 1:2, y2 = 3:4)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunction$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
