# Sequential Multi-Objective Bayesian Optimization via SMS-EGO

Loop function for sequential multi-objective Bayesian Optimization via
SMS-EGO. Normally used inside an
[OptimizerMbo](https://mlr3mbo.mlr-org.com/dev/reference/mlr_optimizers_mbo.md).

In each iteration after the initial design, the surrogate and
acquisition function
([mlr_acqfunctions_smsego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md))
are updated and the next candidate is chosen based on optimizing the
acquisition function.

## Usage

``` r
bayesopt_smsego(
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

  ([SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearnerCollection.md))  
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/dev/reference/SurrogateLearnerCollection.md)
  to be used as a surrogate.

- acq_function:

  ([mlr_acqfunctions_smsego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md))  
  [mlr_acqfunctions_smsego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md)
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

- Due to the iterative computation of the epsilon within the
  [mlr_acqfunctions_smsego](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions_smsego.md),
  requires the
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  of the
  [bbotk::OptimInstanceBatchMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceBatchMultiCrit.html)
  to be a
  [bbotk::TerminatorEvals](https://bbotk.mlr-org.com/reference/mlr_terminators_evals.html).

## References

- Beume N, Naujoks B, Emmerich M (2007). “SMS-EMOA: Multiobjective
  selection based on dominated hypervolume.” *European Journal of
  Operational Research*, **181**(3), 1653–1669.

- Ponweiser, Wolfgang, Wagner, Tobias, Biermann, Dirk, Vincze, Markus
  (2008). “Multiobjective Optimization on a Limited Budget of
  Evaluations Using Model-Assisted S-Metric Selection.” In *Proceedings
  of the 10th International Conference on Parallel Problem Solving from
  Nature*, 784–794.

## See also

Other Loop Function:
[`loop_function`](https://mlr3mbo.mlr-org.com/dev/reference/loop_function.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions.md),
[`mlr_loop_functions_ego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_ego.md),
[`mlr_loop_functions_emo`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_emo.md),
[`mlr_loop_functions_mpcl`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_mpcl.md),
[`mlr_loop_functions_parego`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions_parego.md)

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

  acq_function = acqf("smsego")

  acq_optimizer = acqo(
    optimizer = opt("random_search", batch_size = 100),
    terminator = trm("evals", n_evals = 100))

  optimizer = opt("mbo",
    loop_function = bayesopt_smsego,
    surrogate = surrogate,
    acq_function = acq_function,
    acq_optimizer = acq_optimizer)

  optimizer$optimize(instance)
}
#>            x  x_domain        y1        y2
#>        <num>    <list>     <num>     <num>
#> 1: 1.1886716 <list[1]> 1.4129401 0.6582538
#> 2: 0.8789091 <list[1]> 0.7724811 1.2568449
# }
```
