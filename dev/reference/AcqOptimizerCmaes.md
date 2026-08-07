# CMA-ES Acquisition Function Optimizer

CMA-ES acquisition function optimizer. Calls `cmaes()` from
[libcmaesr](https://CRAN.R-project.org/package=libcmaesr). The default
algorithm is `"abipop"` with unlimited restarts and a budget of
`100 * D^2` function evaluations, where `D` is the dimension of the
search space. The optimization starts from the best point in the
archive. For the meaning of the control parameters, see
[`libcmaesr::cmaes_control()`](https://libcmaesr.mlr-org.com/reference/cmaes_control.html).

Only fully numeric search spaces (all parameters of type `p_dbl`) are
supported.

## Parameters

- `algo`:

  `character(1)`  
  CMA-ES variant to use, see
  [`libcmaesr::cmaes_algos`](https://libcmaesr.mlr-org.com/reference/cmaes_algos.html).
  Default is `"abipop"`.

- `lambda`:

  `integer(1)`  
  Number of generated descendants per iteration. Deactivate with `NA`
  (Default).

- `sigma`:

  `numeric(1)`  
  Initial sigma for the covariance. Deactivate with `NA` (Default).

- `max_restarts`:

  `integer(1)`  
  Maximum number of restarts for the IPOP and BIPOP variants. Default is
  `1e5`, i.e., restarts are only limited by the evaluation budget.
  Deactivate with `NA`.

- `tpa`:

  `integer(1)`  
  Activates or deactivates the two-point adaptation step-size mechanism.
  `0` for no, `1` for auto, `2` for yes. Deactivate with `NA` (Default).

- `tpa_dsigma`:

  `numeric(1)`  
  Value of the two-point adaptation dsigma. Deactivate with `NA`
  (Default).

- `seed`:

  `integer(1)`  
  Seed of the random number generator of libcmaes. If `NA` (Default),
  the seed is drawn from R and the optimization is therefore
  reproducible via [`set.seed()`](https://rdrr.io/r/base/Random.html).

- `quiet`:

  `logical(1)`  
  Should the output of libcmaes be suppressed? Default is `TRUE`.

- `skip_already_evaluated`:

  `logical(1)`  
  Should the proposed candidate be rejected if it was already evaluated
  on the actual
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)?
  If `TRUE` and the candidate was already evaluated, an error is raised
  so that the `loop_function` can propose a randomly sampled point
  instead. Default is `TRUE`.

## Termination Parameters

The following termination parameters can be used.

- `max_fevals`:

  `integer(1)`  
  Maximum number of function evaluations. Default is `100 * D^2`, where
  `D` is the dimension of the search space. Deactivate with `NA`.

- `max_iter`:

  `integer(1)`  
  Maximum number of iterations. Deactivate with `NA` (Default).

- `ftarget`:

  `numeric(1)`  
  Target function value. Deactivate with `NA` (Default).

- `f_tolerance`:

  `numeric(1)`  
  Function tolerance. Deactivate with `NA` (Default).

- `x_tolerance`:

  `numeric(1)`  
  Parameter tolerance. Deactivate with `NA` (Default).

## Super class

[`AcqOptimizer`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
-\> `AcqOptimizerCmaes`

## Public fields

- `state`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Result of the last
  [`libcmaesr::cmaes()`](https://libcmaesr.mlr-org.com/reference/cmaes.html)
  run.

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object.

## Methods

### Public methods

- [`AcqOptimizerCmaes$new()`](#method-AcqOptimizerCmaes-initialize)

- [`AcqOptimizerCmaes$optimize()`](#method-AcqOptimizerCmaes-optimize)

- [`AcqOptimizerCmaes$reset()`](#method-AcqOptimizerCmaes-reset)

- [`AcqOptimizerCmaes$clone()`](#method-AcqOptimizerCmaes-clone)

Inherited methods

- [`AcqOptimizer$format()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-format)
- [`AcqOptimizer$print()`](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.html#method-print)

------------------------------------------------------------------------

### `AcqOptimizerCmaes$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AcqOptimizerCmaes$new(acq_function = NULL)

#### Arguments

- `acq_function`:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)).

------------------------------------------------------------------------

### `AcqOptimizerCmaes$optimize()`

Optimize the acquisition function.

#### Usage

    AcqOptimizerCmaes$optimize()

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with 1 row per candidate.

------------------------------------------------------------------------

### `AcqOptimizerCmaes$reset()`

Reset the acquisition function optimizer.

Clears the `state` of the previous optimization run.

#### Usage

    AcqOptimizerCmaes$reset()

------------------------------------------------------------------------

### `AcqOptimizerCmaes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AcqOptimizerCmaes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (requireNamespace("libcmaesr")) {
  acqo("cmaes")
}
#> <AcqOptimizerCmaes>: (OptimizerCmaes)
#> * Parameters: algo=abipop, max_restarts=100000,
#>   skip_already_evaluated=TRUE, catch_errors=TRUE
```
