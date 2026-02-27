# Sequential Multi-Objective Bayesian Optimization

Loop function for sequential multi-objective Bayesian Optimization.
Normally used inside an
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).
The conceptual counterpart to
[mlr_loop_functions_ego](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md).

In each iteration after the initial design, the surrogate and
acquisition function are updated and the next candidate is chosen based
on optimizing the acquisition function.

## Usage

``` r
bayesopt_emo(
  instance,
  surrogate,
  acq_function,
  acq_optimizer,
  init_design_size = NULL,
  random_interleave_iter = 0L
)
```

## Arguments

- instance:

  ([bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html))  
  The
  [bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html)
  to be optimized.

- surrogate:

  ([SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md))  
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)
  to be used as a surrogate.

- acq_function:

  ([AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md))  
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md) to
  be used as acquisition function.

- acq_optimizer:

  ([AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md))  
  [AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
  to be used as acquisition function optimizer.

- init_design_size:

  (`NULL` \| `integer(1)`)  
  Size of the initial design. If `NULL` and the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  contains no evaluations, `4 * d` is used with `d` being the
  dimensionality of the search space. Points are generated via a Sobol
  sequence.

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
  [bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html).

## See also

Other Loop Function:
[`loop_function`](https://mlr3mbo.mlr-org.com/reference/loop_function.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md),
[`mlr_loop_functions_ego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md),
[`mlr_loop_functions_mpcl`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_mpcl.md),
[`mlr_loop_functions_parego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_parego.md),
[`mlr_loop_functions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_smsego.md)

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
    list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchMultiCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  surrogate = default_surrogate(instance)

  acq_function = acqf("ehvi")

  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))

  optimizer = opt("mbo",
    loop_function = bayesopt_emo,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  optimizer$optimize(instance)
}
#>             x  x_domain         y1       y2
#>         <num>    <list>      <num>    <num>
#> 1:  4.2306054 <list[1]> 17.8980217 4.975600
#> 2: -0.7448662 <list[1]>  0.5548257 7.534291
# }
```
