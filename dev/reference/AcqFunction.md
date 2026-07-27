# Acquisition Function Base Class

Abstract acquisition function class.

Based on the predictions of a
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md), the
acquisition function encodes the preference to evaluate a new point.

Most acquisition functions are stateful and depend on quantities that
must be recomputed whenever the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) has
been refitted on new data, e.g., the best objective function value
observed so far (`$y_best` of
[mlr_acqfunctions_ei](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ei.md))
or the current Pareto front and the reference point (`$ys_front` and
`$ref_point` of
[mlr_acqfunctions_ehvi](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ehvi.md)).
These quantities are cached in public fields and are recomputed from the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) and
its [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html)
by calling `$update()`. Which fields a subclass sets is documented in
its `$update()` method.

Loop functions such as
[bayesopt_ego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_ego.md)
call `$update()` in every iteration, directly after updating the
surrogate and before optimizing the acquisition function:

    acq_function$surrogate$update()
    acq_function$update()
    acq_optimizer$optimize()

The order matters, because `$update()` reads the archive through the
surrogate and may rely on the surrogate's predictions or its
[OutputTrafo](https://mlr3mbo.mlr-org.com/dev/reference/OutputTrafo.md).
Evaluating an acquisition function whose cached fields have not been set
results in an error along the lines of
`"$y_best is not set. Missed to call $update()?"`.

`$reset()` discards state so that the same acquisition function object
can be reused for another optimization run without carrying over
information from the previous one. Fields that `$update()` recomputes
from scratch in every iteration need not be reset, which is why most
acquisition functions do not override `$reset()`. It matters for state
that persists across iterations instead:
[mlr_acqfunctions_stochastic_cb](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_cb.md),
for example, samples `lambda` once at the first `$update()` and
afterwards only decays it using an iteration counter, so both are reset
to make the next run start from a freshly sampled `lambda`.
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md)
and
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_async_mbo.md)
call `$reset()` at the beginning of `$optimize()`, together with
resetting the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) and
the
[AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md).

Both methods can be implemented by subclasses. The default
implementations do nothing, which is sufficient for stateless
acquisition functions such as
[mlr_acqfunctions_mean](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_mean.md)
or
[mlr_acqfunctions_sd](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_sd.md).

## See also

Other Acquisition Function:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ei.md),
[`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ei_log.md),
[`mlr_acqfunctions_eips`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_eips.md),
[`mlr_acqfunctions_mean`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_mean.md),
[`mlr_acqfunctions_multi`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_multi.md),
[`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_pi.md),
[`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_sd.md),
[`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md),
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_ei.md)

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

  ([Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md))  
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

- [`AcqFunction$new()`](#method-AcqFunction-initialize)

- [`AcqFunction$update()`](#method-AcqFunction-update)

- [`AcqFunction$reset()`](#method-AcqFunction-reset)

- [`AcqFunction$eval_many()`](#method-AcqFunction-eval_many)

- [`AcqFunction$eval_dt()`](#method-AcqFunction-eval_dt)

- [`AcqFunction$assert_surrogate()`](#method-AcqFunction-assert_surrogate)

- [`AcqFunction$clone()`](#method-AcqFunction-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)

------------------------------------------------------------------------

### `AcqFunction$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that the surrogate can be initialized lazy and can later be set via
the active binding `$surrogate`.

#### Usage

    AcqFunction$new(
      id,
      constants = ParamSet$new(),
      surrogate = NULL,
      requires_predict_type_se,
      surrogate_class,
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
  [Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)).
  Surrogate whose predictions are used in the acquisition function.

- `requires_predict_type_se`:

  (`logical(1)`)  
  Whether the acquisition function requires the surrogate to have `"se"`
  as `$predict_type`.

- `surrogate_class`:

  (`character(1)`)  
  Allowed class of the surrogate.

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

### `AcqFunction$update()`

Update the acquisition function. Recomputes the cached quantities from
the current state of the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) and
its [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html).
Can be implemented by subclasses; see the class description above for
details.

#### Usage

    AcqFunction$update()

#### Returns

`NULL`.

------------------------------------------------------------------------

### `AcqFunction$reset()`

Reset the acquisition function. Discards state so that the acquisition
function can be reused for another optimization run. Can be implemented
by subclasses; see the class description above for details.

#### Usage

    AcqFunction$reset()

#### Returns

`NULL`.

------------------------------------------------------------------------

### `AcqFunction$eval_many()`

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

### `AcqFunction$eval_dt()`

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

### `AcqFunction$assert_surrogate()`

Validate that the surrogate is compatible with this acquisition
function. Asserts the surrogate class and that `$predict_type` is `"se"`
if required. Subclasses with additional requirements must override this
method.

#### Usage

    AcqFunction$assert_surrogate(surrogate)

#### Arguments

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md))  
  Surrogate to validate.

#### Returns

The validated
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md).

------------------------------------------------------------------------

### `AcqFunction$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqFunction$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
