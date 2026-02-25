# Asynchronous Model Based Optimization

`OptimizerAsyncMbo` class that implements Asynchronous Model Based
Optimization (AMBO). AMBO starts multiple sequential MBO runs on
different workers. The worker communicate asynchronously through a
shared archive relying on the
[rush](https://CRAN.R-project.org/package=rush) package. The optimizer
follows a modular layout in which the surrogate model, acquisition
function, and acquisition optimizer can be changed. The
[SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)
will impute missing values due to pending evaluations. A stochastic
[AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md),
e.g.,
[AcqFunctionStochasticEI](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_ei.md)
or
[AcqFunctionStochasticCB](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_stochastic_cb.md)
is used to create varying versions of the acquisition function on each
worker, promoting different exploration-exploitation trade-offs. The
[AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
class remains consistent with the one used in synchronous MBO.

In contrast to
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md),
no
[loop_function](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md)
can be specified that determines the AMBO flavor as `OptimizerAsyncMbo`
simply relies on a surrogate update, acquisition function update and
acquisition function optimization step as an internal loop.

Currently, only single-objective optimization is supported and
`OptimizerAsyncMbo` is considered an experimental feature and API might
be subject to changes.

Note that in general the
[SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)
is updated one final time on all available data after the optimization
process has terminated. However, in certain scenarios this is not always
possible or meaningful. It is therefore recommended to manually inspect
the
[SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)
after optimization if it is to be used, e.g., for visualization purposes
to make sure that it has been properly updated on all available data. If
this final update of the
[SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)
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
[bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
holds the following additional columns that are specific to AMBO
algorithms:

- `acq_function$id` (`numeric(1)`)  
  The value of the acquisition function.

- `".already_evaluated"` (`logical(1))`  
  Whether this point was already evaluated. Depends on the
  `skip_already_evaluated` parameter of the
  [AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md).

If the
[bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
does not contain any evaluations prior to optimization, an initial
design is needed. If the `initial_design` parameter is specified to be a
`data.table`, this data will be used. Otherwise, if it is `NULL`, an
initial design of size `design_size` will be generated based on the
`generate_design` sampling function. See also the parameters below.

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

[`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
-\>
[`bbotk::OptimizerAsync`](https://bbotk.mlr-org.com/reference/OptimizerAsync.html)
-\> `OptimizerAsyncMbo`

## Active bindings

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

- [`OptimizerAsyncMbo$new()`](#method-OptimizerAsyncMbo-new)

- [`OptimizerAsyncMbo$print()`](#method-OptimizerAsyncMbo-print)

- [`OptimizerAsyncMbo$reset()`](#method-OptimizerAsyncMbo-reset)

- [`OptimizerAsyncMbo$optimize()`](#method-OptimizerAsyncMbo-optimize)

- [`OptimizerAsyncMbo$clone()`](#method-OptimizerAsyncMbo-clone)

Inherited methods

- [`bbotk::Optimizer$format()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-format)
- [`bbotk::Optimizer$help()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

If `surrogate` is `NULL` and the `acq_function$surrogate` field is
populated, this
[SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)
is used. Otherwise, `default_surrogate(instance)` is used. If
`acq_function` is `NULL` and the `acq_optimizer$acq_function` field is
populated, this
[AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
is used (and therefore its `$surrogate` if populated; see above).
Otherwise `default_acqfunction(instance)` is used. If `acq_optimizer` is
`NULL`, `default_acqoptimizer(instance)` is used.

Even if already initialized, the `surrogate$archive` field will always
be overwritten by the
[bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
of the current
[bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
to be optimized.

For more information on default values for `surrogate`, `acq_function`,
`acq_optimizer` and `result_assigner`, see
[`?mbo_defaults`](https://mlr3mbo.mlr-org.com/dev/reference/mbo_defaults.md).

#### Usage

    OptimizerAsyncMbo$new(
      id = "async_mbo",
      surrogate = NULL,
      acq_function = NULL,
      acq_optimizer = NULL,
      result_assigner = NULL,
      param_set = NULL,
      label = "Asynchronous Model Based Optimization",
      man = "mlr3mbo::OptimizerAsyncMbo"
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

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

- `result_assigner`:

  ([ResultAssigner](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md)
  \| `NULL`)  
  The result assigner.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    OptimizerAsyncMbo$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `reset()`

Reset the optimizer. Sets the following fields to `NULL`: `surrogate`,
`acq_function`, `acq_optimizer`,`result_assigner` Resets parameter
values `design_size` and `design_function` to their defaults.

#### Usage

    OptimizerAsyncMbo$reset()

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the optimization on an
[bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
until termination. The single evaluations will be written into the
[bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html).
The result will be written into the instance object.

#### Usage

    OptimizerAsyncMbo$optimize(inst)

#### Arguments

- `inst`:

  ([bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerAsyncMbo$clone(deep = FALSE)

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

    library(bbotk)
    library(paradox)
    library(mlr3learners)

    fun = function(xs) {
      list(y = xs$x ^ 2)
    }
    domain = ps(x = p_dbl(lower = -10, upper = 10))
    codomain = ps(y = p_dbl(tags = "minimize"))
    objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

    instance = OptimInstanceAsyncSingleCrit$new(
      objective = objective,
      terminator = trm("evals", n_evals = 10))

    mirai::daemons(2)
    rush::rush_plan(n_workers=2, worker_type = "remote")

    optimizer = opt("async_mbo", design_size = 4, n_workers = 2)

    optimizer$optimize(instance)
    mirai::daemons(0)
  } else {
    message("Redis server is not available.\nPlease set up Redis prior to running the example.")
  }
}
#> Redis server is not available.
#> Please set up Redis prior to running the example.
# }
```
