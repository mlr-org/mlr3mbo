# Multi-Objective Bayesian Optimization via ParEGO

Loop function for multi-objective Bayesian Optimization via ParEGO.
Normally used inside an
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md).

In each iteration after the initial design, the observed objective
function values are normalized and `q` candidates are obtained by
scalarizing these values via the augmented Tchebycheff function,
updating the surrogate with respect to these scalarized values and
optimizing the acquisition function.

## Usage

``` r
bayesopt_parego(
  instance,
  surrogate,
  acq_function,
  acq_optimizer,
  init_design_size = NULL,
  q = 1L,
  s = 100L,
  rho = 0.05,
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

  ([SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md))  
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearner.md)
  to be used as a surrogate.

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
  Batch size, i.e., the number of candidates to be obtained for a single
  batch. Default is `1`.

- s:

  (`integer(1)`)  
  \\s\\ in Equation 1 in Knowles (2006). Determines the total number of
  possible random weight vectors. Default is `100`.

- rho:

  (`numeric(1)`)  
  \\\rho\\ in Equation 2 in Knowles (2006) scaling the linear part of
  the augmented Tchebycheff function. Default is `0.05`

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

- The scalarizations of the objective function values are stored as the
  `y_scal` column in the
  [bbotk::ArchiveBatch](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
  of the
  [bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html).

- To make use of parallel evaluations in the case of \`q \> 1, the
  objective function of the
  [bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html)
  must be implemented accordingly.

## References

- Knowles, Joshua (2006). “ParEGO: A Hybrid Algorithm With On-Line
  Landscape Approximation for Expensive Multiobjective Optimization
  Problems.” *IEEE Transactions on Evolutionary Computation*, **10**(1),
  50–66.

## See also

Other Loop Function:
[`loop_function`](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions.md),
[`mlr_loop_functions_ego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_ego.md),
[`mlr_loop_functions_emo`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_emo.md),
[`mlr_loop_functions_mpcl`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_mpcl.md),
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
    list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchMultiCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  surrogate = default_surrogate(instance, n_learner = 1)

  acq_function = acqf("ei")

  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))

  optimizer = opt("mbo",
    loop_function = bayesopt_parego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  optimizer$optimize(instance)
}
#> WARN  [12:20:35.939] [bbotk] 
#> ✖ Task 'surrogate_task' has missing values in column(s) 'y_scal', but learner
#>   'regr.km' does not support this
#> → Class: Mlr3ErrorInput
#> 
#> WARN  [12:20:35.968] [bbotk] Could not update the surrogate a final time after the optimization process has terminated.
#>             x  x_domain           y1       y2
#>         <num>    <list>        <num>    <num>
#> 1: -0.0181669 <list[1]> 0.0003300362 4.072998
# }
```
