# Single-Objective Bayesian Optimization via Multipoint Constant Liar

Loop function for single-objective Bayesian Optimization via multipoint
constant liar. Normally used inside an
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md).

In each iteration after the initial design, the surrogate and
acquisition function are updated. The acquisition function is then
optimized, to find a candidate but instead of evaluating this candidate,
the objective function value is obtained by applying the `liar` function
to all previously obtained objective function values. This is repeated
`q - 1` times to obtain a total of `q` candidates that are then
evaluated in a single batch.

## Usage

``` r
bayesopt_mpcl(
  instance,
  surrogate,
  acq_function,
  acq_optimizer,
  init_design_size = NULL,
  q = 2L,
  liar = mean,
  random_interleave_iter = 0L
)
```

## Arguments

- instance:

  ([bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html))  
  The
  [bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
  to be optimized.

- surrogate:

  ([Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md))  
  [Surrogate](https://mlr3mbo.mlr-org.com/dev/reference/Surrogate.md) to
  be used as a surrogate. Typically a
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md).

- acq_function:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md))  
  [AcqFunction](https://mlr3mbo.mlr-org.com/dev/reference/AcqFunction.md)
  to be used as acquisition function.

- acq_optimizer:

  ([AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md))  
  [AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md)
  to be used as acquisition function optimizer.

- init_design_size:

  (`NULL` \| `integer(1)`)  
  Size of the initial design. If `NULL` and the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  contains no evaluations, `4 * d` is used with `d` being the
  dimensionality of the search space. Points are generated via a Sobol
  sequence.

- q:

  (`integer(1)`)  
  Batch size \> `1`. Default is `2`.

- liar:

  (`function`)  
  Any function accepting a numeric vector as input and returning a
  single numeric output. Default is `mean`. Other sensible functions
  include `min` (or `max`, depending on the optimization direction).

- random_interleave_iter:

  (`integer(1)`)  
  Every `random_interleave_iter` iteration (starting after the initial
  design), a point is sampled uniformly at random and evaluated (instead
  of a model based proposal). For example, if
  `random_interleave_iter = 2`, random interleaving is performed in the
  second, fourth, sixth, ... iteration. Default is `0`, i.e., no random
  interleaving is performed at all.

## Value

invisible(instance)  
The original instance is modified in-place and returned invisible.

## Note

- The `acq_function$surrogate`, even if already populated, will always
  be overwritten by the `surrogate`.

- The `acq_optimizer$acq_function`, even if already populated, will
  always be overwritten by `acq_function`.

- The `surrogate$archive`, even if already populated, will always be
  overwritten by the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  of the
  [bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html).

- To make use of parallel evaluations in the case of \`q \> 1, the
  objective function of the
  [bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
  must be implemented accordingly.

## References

- Ginsbourger, David, Le Riche, Rodolphe, Carraro, Laurent (2008). “A
  Multi-Points Criterion for Deterministic Parallel Global Optimization
  Based on Gaussian Processes.”

- Wang, Jialei, Clark, C. S, Liu, Eric, Frazier, I. P (2020). “Parallel
  Bayesian Global Optimization of Expensive Functions.” *Operations
  Research*, **68**(6), 1850–1865.

## See also

Other Loop Function:
[`loop_function`](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions.md),
[`mlr_loop_functions_ego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_ego.md),
[`mlr_loop_functions_emo`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_emo.md),
[`mlr_loop_functions_parego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_parego.md),
[`mlr_loop_functions_smsego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_smsego.md)

## Examples

``` r
# \donttest{
if (requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {

  library(bbotk)
  library(paradox)
  library(mlr3learners)

  fun = function(xs) {
    list(y = xs$x ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 7))

  surrogate = default_surrogate(instance)

  acq_function = acqf("ei")

  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))

  optimizer = opt("mbo",
    loop_function = bayesopt_mpcl,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer,
    args = list(q = 3))

  optimizer$optimize(instance)
}
#>            x  x_domain         y
#>        <num>    <list>     <num>
#> 1: 0.3457214 <list[1]> 0.1195233
# }
```
