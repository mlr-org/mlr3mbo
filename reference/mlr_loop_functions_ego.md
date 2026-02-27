# Sequential Single-Objective Bayesian Optimization

Loop function for sequential single-objective Bayesian Optimization.
Normally used inside an
[OptimizerMbo](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md).

In each iteration after the initial design, the surrogate and
acquisition function are updated and the next candidate is chosen based
on optimizing the acquisition function.

## Usage

``` r
bayesopt_ego(
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

  ([bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html))  
  The
  [bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
  to be optimized.

- surrogate:

  ([Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md))  
  [Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.md) to be
  used as a surrogate. Typically a
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md).

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
  [bbotk::OptimInstanceBatchSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html).

## References

- Jones, R. D, Schonlau, Matthias, Welch, J. W (1998). “Efficient Global
  Optimization of Expensive Black-Box Functions.” *Journal of Global
  optimization*, **13**(4), 455–492.

- Snoek, Jasper, Larochelle, Hugo, Adams, P R (2012). “Practical
  Bayesian Optimization of Machine Learning Algorithms.” In Pereira F,
  Burges CJC, Bottou L, Weinberger KQ (eds.), *Advances in Neural
  Information Processing Systems*, volume 25, 2951–2959.

## See also

Other Loop Function:
[`loop_function`](https://mlr3mbo.mlr-org.com/reference/loop_function.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md),
[`mlr_loop_functions_emo`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_emo.md),
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

  # expected improvement per second example
  fun = function(xs) {
    list(y = xs$x ^ 2, time = abs(xs$x))
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  surrogate = default_surrogate(instance, n_learner = 2)
  surrogate$cols_y = c("y", "time")

  optimizer = opt("mbo",
    loop_function = bayesopt_ego,
    surrogate = surrogate,
    acq_function = acqf("eips"),
    acq_optimizer = acq_optimizer)

  optimizer$optimize(instance)
}
#>           x  x_domain        y
#>       <num>    <list>    <num>
#> 1: 2.046084 <list[1]> 4.186461
# }
```
