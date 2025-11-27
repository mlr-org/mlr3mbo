# Model Based Optimization

`OptimizerMbo` class that implements Model Based Optimization (MBO). The
implementation follows a modular layout relying on a
[loop_function](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md)
determining the MBO flavor to be used, e.g.,
[bayesopt_ego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_ego.md)
for sequential single-objective Bayesian Optimization, a
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md), an
[AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md),
e.g.,
[mlr_acqfunctions_ei](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_ei.md)
for Expected Improvement and an
[AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md).

MBO algorithms are iterative optimization algorithms that make use of a
continuously updated surrogate model built for the objective function.
By optimizing a comparably cheap to evaluate acquisition function
defined on the surrogate prediction, the next candidate is chosen for
evaluation.

Detailed descriptions of different MBO flavors are provided in the
documentation of the respective
[loop_function](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md).

Termination is handled via a
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
part of the
[bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
to be optimized.

Note that in general the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) is
updated one final time on all available data after the optimization
process has terminated. However, in certain scenarios this is not always
possible or meaningful, e.g., when using
[`bayesopt_parego()`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_parego.md)
for multi-objective optimization which uses a surrogate that relies on a
scalarization of the objectives. It is therefore recommended to manually
inspect the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)
after optimization if it is to be used, e.g., for visualization purposes
to make sure that it has been properly updated on all available data. If
this final update of the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)
could not be performed successfully, a warning will be logged.

By specifying a
[ResultAssigner](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md),
one can alter how the final result is determined after optimization,
e.g., simply based on the evaluations logged in the archive
[ResultAssignerArchive](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners_archive.md)
or based on the
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) via
[ResultAssignerSurrogate](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners_surrogate.md).

## Archive

The
[bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
holds the following additional columns that are specific to MBO
algorithms:

- `acq_function$id` (`numeric(1)`)  
  The value of the acquisition function.

- `".already_evaluated"` (`logical(1))`  
  Whether this point was already evaluated. Depends on the
  `skip_already_evaluated` parameter of the
  [AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md).

## Super classes

[`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
-\>
[`bbotk::OptimizerBatch`](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
-\> `OptimizerMbo`

## Active bindings

- `loop_function`:

  ([loop_function](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md)
  \| `NULL`)  
  Loop function determining the MBO flavor.

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)
  \| `NULL`)  
  The surrogate.

- `acq_function`:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
  \| `NULL`)  
  The acquisition function.

- `acq_optimizer`:

  ([AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
  \| `NULL`)  
  The acquisition function optimizer.

- `args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Further arguments passed to the `loop_function`. For example,
  `random_interleave_iter`.

- `result_assigner`:

  ([ResultAssigner](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md)
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

- [`OptimizerMbo$new()`](#method-OptimizerMbo-new)

- [`OptimizerMbo$print()`](#method-OptimizerMbo-print)

- [`OptimizerMbo$reset()`](#method-OptimizerMbo-reset)

- [`OptimizerMbo$optimize()`](#method-OptimizerMbo-optimize)

- [`OptimizerMbo$clone()`](#method-OptimizerMbo-clone)

Inherited methods

- [`bbotk::Optimizer$format()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-format)
- [`bbotk::Optimizer$help()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

If `surrogate` is `NULL` and the `acq_function$surrogate` field is
populated, this
[Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) is
used. Otherwise, `default_surrogate(instance)` is used. If
`acq_function` is `NULL` and the `acq_optimizer$acq_function` field is
populated, this
[AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
is used (and therefore its `$surrogate` if populated; see above).
Otherwise `default_acqfunction(instance)` is used. If `acq_optimizer` is
`NULL`, `default_acqoptimizer(instance)` is used.

Even if already initialized, the `surrogate$archive` field will always
be overwritten by the
[bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
of the current
[bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
to be optimized.

For more information on default values for `loop_function`, `surrogate`,
`acq_function`, `acq_optimizer` and `result_assigner`, see
[`?mbo_defaults`](https://mlr3mbo.mlr-org.com/dev/reference/mbo_defaults.md).

#### Usage

    OptimizerMbo$new(
      loop_function = NULL,
      surrogate = NULL,
      acq_function = NULL,
      acq_optimizer = NULL,
      args = NULL,
      result_assigner = NULL
    )

#### Arguments

- `loop_function`:

  ([loop_function](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md)
  \| `NULL`)  
  Loop function determining the MBO flavor.

- `surrogate`:

  ([Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md)
  \| `NULL`)  
  The surrogate.

- `acq_function`:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
  \| `NULL`)  
  The acquisition function.

- `acq_optimizer`:

  ([AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
  \| `NULL`)  
  The acquisition function optimizer.

- `args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Further arguments passed to the `loop_function`. For example,
  `random_interleave_iter`.

- `result_assigner`:

  ([ResultAssigner](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md)
  \| `NULL`)  
  The result assigner.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    OptimizerMbo$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `reset()`

Reset the optimizer. Sets the following fields to `NULL`:
`loop_function`, `surrogate`, `acq_function`, `acq_optimizer`, `args`,
`result_assigner`

#### Usage

    OptimizerMbo$reset()

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the optimization and writes optimization result into
[bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html).
The optimization result is returned but the complete optimization path
is stored in
[bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
of
[bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html).

#### Usage

    OptimizerMbo$optimize(inst)

#### Arguments

- `inst`:

  ([bbotk::OptimInstanceBatch](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)).

#### Returns

[data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerMbo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
if (requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {

  library(bbotk)
  library(paradox)
  library(mlr3learners)

  # single-objective EGO
  fun = function(xs) {
    list(y = xs$x ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  surrogate = default_surrogate(instance)

  acq_function = acqf("ei")

  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))

  optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  optimizer$optimize(instance)

  # multi-objective ParEGO
  fun = function(xs) {
    list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchMultiCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  optimizer = opt("mbo",
    loop_function = bayesopt_parego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  optimizer$optimize(instance)
}
#> WARN  [09:05:34.743] [bbotk] Task 'surrogate_task' has missing values in column(s) 'y_scal', but learner 'regr.km' does not support this
#> WARN  [09:05:34.744] [bbotk] Could not update the surrogate a final time after the optimization process has terminated.
#>             x  x_domain         y1       y2
#>         <num>    <list>      <num>    <num>
#> 1: -0.9576857 <list[1]>  0.9171619 8.747905
#> 2:  4.0423143 <list[1]> 16.3403048 4.171048
# }
```
