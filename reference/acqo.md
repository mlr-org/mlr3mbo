# Syntactic Sugar Acquisition Function Optimizer Construction

This function allows to construct an
[AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md) in
the spirit of `mlr_sugar` from
[mlr3](https://CRAN.R-project.org/package=mlr3).

## Usage

``` r
acqo(optimizer, terminator, acq_function = NULL, callbacks = NULL, ...)
```

## Arguments

- optimizer:

  ([bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
  \| `character(1)`)  
  [bbotk::OptimizerBatch](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
  that is to be used or the name of the optimizer in
  [mlr_acqoptimizers](https://mlr3mbo.mlr-org.com/reference/mlr_acqoptimizers.md).

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  that is to be used.

- acq_function:

  (`NULL` \|
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md))  
  [AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
  that is to be used. Can also be `NULL`.

- callbacks:

  (`NULL` \| list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))
  Callbacks used during acquisition function optimization.

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to be set as parameters in
  the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html).

## Value

[AcqOptimizer](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)

## Examples

``` r
library(bbotk)
acqo(opt("random_search"), trm("evals"), catch_errors = FALSE)
#> <AcqOptimizer>: (OptimizerBatchRandomSearch | TerminatorEvals)
#> * Parameters: n_candidates=1, logging_level=warn, warmstart=FALSE,
#>   skip_already_evaluated=TRUE, catch_errors=FALSE
```
