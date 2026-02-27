# Asynchronous Decentralized Bayesian Optimization

`OptimizerADBO` class that implements Asynchronous Decentralized
Bayesian Optimization (ADBO). ADBO is a variant of Asynchronous Model
Based Optimization (AMBO) that uses
[AcqFunctionStochasticCB](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md)
with exponential lambda decay.

Currently, only single-objective optimization is supported and
OptimizerADBO is considered an experimental feature and API might be
subject to changes.

## Note

The lambda parameter of the confidence bound acquisition function
controls the trade-off between exploration and exploitation. A large
lambda value leads to more exploration, while a small lambda value leads
to more exploitation. The initial lambda value of the acquisition
function used on each worker is drawn from an exponential distribution
with rate `1 / lambda`. ADBO can use periodic exponential decay to
reduce lambda periodically for a given time step `t` with the formula
`lambda * exp(-rate * (t %% period))`. The
[SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
is configured to use a random forest and the
[AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md) is
a random search with a batch size of 1000 and a budget of 10000
evaluations.

## Parameters

- `lambda`:

  `numeric(1)`  
  Value used for sampling the lambda for each worker from an exponential
  distribution.

- `rate`:

  `numeric(1)`  
  Rate of the exponential decay.

- `period`:

  `integer(1)`  
  Period of the exponential decay.

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

## References

- Egelé, Romain, Guyon, Isabelle, Vishwanath, Venkatram, Balaprakash,
  Prasanna (2023). “Asynchronous Decentralized Bayesian Optimization for
  Large Scale Hyperparameter Optimization.” In *2023 IEEE 19th
  International Conference on e-Science (e-Science)*, 1–10.

## Super classes

[`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
-\>
[`bbotk::OptimizerAsync`](https://bbotk.mlr-org.com/reference/OptimizerAsync.html)
-\>
[`mlr3mbo::OptimizerAsyncMbo`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md)
-\> `OptimizerADBO`

## Methods

### Public methods

- [`OptimizerADBO$new()`](#method-OptimizerADBO-new)

- [`OptimizerADBO$optimize()`](#method-OptimizerADBO-optimize)

- [`OptimizerADBO$clone()`](#method-OptimizerADBO-clone)

Inherited methods

- [`bbotk::Optimizer$format()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-format)
- [`bbotk::Optimizer$help()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-help)
- [`mlr3mbo::OptimizerAsyncMbo$print()`](https://mlr3mbo.mlr-org.com/reference/OptimizerAsyncMbo.html#method-print)
- [`mlr3mbo::OptimizerAsyncMbo$reset()`](https://mlr3mbo.mlr-org.com/reference/OptimizerAsyncMbo.html#method-reset)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OptimizerADBO$new()

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the optimization on an
[bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
until termination. The single evaluations will be written into the
[bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html).
The result will be written into the instance object.

#### Usage

    OptimizerADBO$optimize(inst)

#### Arguments

- `inst`:

  ([bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerADBO$clone(deep = FALSE)

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

    optimizer = opt("adbo", design_size = 4, n_workers = 2)

    optimizer$optimize(instance)
    mirai::daemons(0)
  } else {
    message("Redis server is not available.\nPlease set up Redis prior to running the example.")
  }
}
#> Loading required namespace: rush
#> Redis server is not available.
#> Please set up Redis prior to running the example.
# }
```
