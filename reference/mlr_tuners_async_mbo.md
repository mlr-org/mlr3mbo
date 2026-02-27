# TunerAsync using Asynchronous Model Based Optimization

`TunerAsyncMbo` class that implements Asynchronous Model Based
Optimization (AMBO). This is a minimal interface internally passing on
to
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md).
For additional information and documentation see
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md).

Currently, only single-objective optimization is supported and
`TunerAsyncMbo` is considered an experimental feature and API might be
subject to changes.

## Parameters

- `initial_design`:

  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Initial design of the optimization. If `NULL`, a design of size
  `design_size` is generated with the specified `design_function`.
  Default is `NULL`.

- `design_size`:

  `integer(1)`  
  Size of the initial design if it is to be generated. Default is `100`.

- `design_function`:

  `character(1)`  
  Sampling function to generate the initial design. Can be `random`
  [paradox::generate_design_random](https://paradox.mlr-org.com/reference/generate_design_random.html),
  `lhs`
  [paradox::generate_design_lhs](https://paradox.mlr-org.com/reference/generate_design_lhs.html),
  or `sobol`
  [paradox::generate_design_sobol](https://paradox.mlr-org.com/reference/generate_design_sobol.html).
  Default is `sobol`.

- `n_workers`:

  `integer(1)`  
  Number of parallel workers. If `NULL`, all rush workers specified via
  [`rush::rush_plan()`](https://rush.mlr-org.com/reference/rush_plan.html)
  are used. Default is `NULL`.

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
-\>
[`mlr3tuning::TunerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsync.html)
-\>
[`mlr3tuning::TunerAsyncFromOptimizerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.html)
-\> `TunerAsyncMbo`

## Active bindings

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md) \|
  `NULL`)  
  The surrogate.

- `acq_function`:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
  \| `NULL`)  
  The acquisition function.

- `acq_optimizer`:

  ([AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
  \| `NULL`)  
  The acquisition function optimizer.

- `result_assigner`:

  ([ResultAssigner](https://mlr3mbo.mlr-org.com/reference/ResultAssigner.md)
  \| `NULL`)  
  The result assigner.

- `param_classes`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported parameter classes that the optimizer can optimize.
  Determined based on the `surrogate` and the `acq_optimizer`. This
  corresponds to the values given by a
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)'s
  `$class` field.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the optimizer. Must be a subset of
  [`bbotk_reflections$optimizer_properties`](https://bbotk.mlr-org.com/reference/bbotk_reflections.html).
  MBO in principle is very flexible and by default we assume that the
  optimizer has all properties. When fully initialized, properties are
  determined based on the loop, e.g., the `loop_function`, and
  `surrogate`.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled prior to optimization
  if at least one of the packages is not installed, but loaded (not
  attached) later on-demand via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html). Required
  packages are determined based on the `acq_function`, `surrogate` and
  the `acq_optimizer`.

## Methods

### Public methods

- [`TunerAsyncMbo$new()`](#method-TunerAsyncMbo-new)

- [`TunerAsyncMbo$print()`](#method-TunerAsyncMbo-print)

- [`TunerAsyncMbo$reset()`](#method-TunerAsyncMbo-reset)

- [`TunerAsyncMbo$clone()`](#method-TunerAsyncMbo-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::TunerAsyncFromOptimizerAsync$optimize()`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. For more
information on default values for `surrogate`, `acq_function`,
`acq_optimizer`, and `result_assigner`, see
[`?mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md).

Note that all the parameters below are simply passed to the
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md)
and the respective fields are simply (settable) active bindings to the
fields of the
[OptimizerAsyncMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md).

#### Usage

    TunerAsyncMbo$new(
      surrogate = NULL,
      acq_function = NULL,
      acq_optimizer = NULL,
      param_set = NULL
    )

#### Arguments

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md) \|
  `NULL`)  
  The surrogate.

- `acq_function`:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
  \| `NULL`)  
  The acquisition function.

- `acq_optimizer`:

  ([AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
  \| `NULL`)  
  The acquisition function optimizer.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    TunerAsyncMbo$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `reset()`

Reset the tuner. Sets the following fields to `NULL`: `surrogate`,
`acq_function`, `acq_optimizer`, `result_assigner` Resets parameter
values `design_size` and `design_function` to their defaults.

#### Usage

    TunerAsyncMbo$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsyncMbo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
if (requireNamespace("rush") &
    requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {

  if (redis_available()) {

    library(mlr3)
    library(mlr3tuning)

    # single-objective
    task = tsk("wine")
    learner = lrn("classif.rpart", cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE))
    resampling = rsmp("cv", folds = 3)
    measure = msr("classif.acc")

    instance = TuningInstanceAsyncSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = measure,
      terminator = trm("evals", n_evals = 10))

    mirai::daemons(2)
    rush::rush_plan(n_workers=2, worker_type = "remote")

    tnr("async_mbo", design_size = 4, n_workers = 2)$optimize(instance)
    mirai::daemons(0)
  } else {
    message("Redis server is not available.\nPlease set up Redis prior to running the example.")
  }
}
#> Redis server is not available.
#> Please set up Redis prior to running the example.
# }
```
