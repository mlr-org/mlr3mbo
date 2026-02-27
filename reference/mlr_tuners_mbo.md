# TunerBatch using Model Based Optimization

`TunerMbo` class that implements Model Based Optimization (MBO). This is
a minimal interface internally passing on to
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).
For additional information and documentation see
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatch.html)
-\>
[`mlr3tuning::TunerBatchFromOptimizerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.html)
-\> `TunerMbo`

## Active bindings

- `loop_function`:

  ([loop_function](https://mlr3mbo.mlr-org.com/reference/loop_function.md)
  \| `NULL`)  
  Loop function determining the MBO flavor.

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

- `args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Further arguments passed to the `loop_function`. For example,
  `random_interleave_iter`.

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

- [`TunerMbo$new()`](#method-TunerMbo-new)

- [`TunerMbo$print()`](#method-TunerMbo-print)

- [`TunerMbo$reset()`](#method-TunerMbo-reset)

- [`TunerMbo$clone()`](#method-TunerMbo-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::TunerBatchFromOptimizerBatch$optimize()`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. For more
information on default values for `loop_function`, `surrogate`,
`acq_function`, `acq_optimizer`, and `result_assigner`, see
[`?mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md).

Note that all the parameters below are simply passed to the
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md)
and the respective fields are simply (settable) active bindings to the
fields of the
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).

#### Usage

    TunerMbo$new(
      loop_function = NULL,
      surrogate = NULL,
      acq_function = NULL,
      acq_optimizer = NULL,
      args = NULL,
      result_assigner = NULL
    )

#### Arguments

- `loop_function`:

  ([loop_function](https://mlr3mbo.mlr-org.com/reference/loop_function.md)
  \| `NULL`)  
  Loop function determining the MBO flavor.

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

- `args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Further arguments passed to the `loop_function`. For example,
  `random_interleave_iter`.

- `result_assigner`:

  ([ResultAssigner](https://mlr3mbo.mlr-org.com/reference/ResultAssigner.md)
  \| `NULL`)  
  The result assigner.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    TunerMbo$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `reset()`

Reset the tuner. Sets the following fields to `NULL`: `loop_function`,
`surrogate`, `acq_function`, `acq_optimizer`, `args`, `result_assigner`

#### Usage

    TunerMbo$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerMbo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
if (requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {

  library(mlr3)
  library(mlr3tuning)

  # single-objective
  task = tsk("wine")
  learner = lrn("classif.rpart", cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE))
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.acc")

  instance = TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    terminator = trm("evals", n_evals = 5))

  tnr("mbo")$optimize(instance)

  # multi-objective
  task = tsk("wine")
  learner = lrn("classif.rpart", cp = to_tune(lower = 1e-4, upper = 1, logscale = TRUE))
  resampling = rsmp("cv", folds = 3)
  measures = msrs(c("classif.acc", "selected_features"))

  instance = TuningInstanceBatchMultiCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measures,
    terminator = trm("evals", n_evals = 5),
    store_models = TRUE) # required due to selected features

  tnr("mbo")$optimize(instance)
}
#>            cp learner_param_vals  x_domain classif.acc selected_features
#>         <num>             <list>    <list>       <num>             <num>
#> 1: -0.7965785          <list[2]> <list[1]>   0.5443503         0.6666667
#> 2: -5.4017490          <list[2]> <list[1]>   0.8993409         2.6666667
#> 3: -3.0991636          <list[2]> <list[1]>   0.8993409         2.6666667
#> 4: -7.7043341          <list[2]> <list[1]>   0.8993409         2.6666667
#> 5: -5.7822485          <list[2]> <list[1]>   0.8993409         2.6666667
# }
```
