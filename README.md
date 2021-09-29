# mlr3mbo

A new R6 and much more modular implementation for single- and multicrit
Bayesian optimization. We are really not done here! Very prelim code, so
beware!

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3mbo/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3mbo/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlr3mbo)](https://cran.r-project.org/package=mlr3mbo)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

[List of some
Ideas](https://github.com/mb706/okmbo/tree/master/todo-files)

## Simple Example

Currently broken

``` r
set.seed(1)
library(bbotk)
library(mlr3)
library(mlr3mbo)
library(paradox)
library(mlr3learners)

obfun = ObjectiveRFun$new(
  fun = function(xs) list(y = sum(unlist(xs)^2)),
  domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
  id = "test"
)

terminator = trm("evals", n_evals = 10)

instance = OptimInstanceSingleCrit$new(
  objective = obfun,
  terminator = terminator
)

design = generate_design_lhs(obfun$domain, 4)$data
instance$eval_batch(design)

surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
acqfun = AcqFunctionEI$new(surrogate = surrogate)
acqopt = AcqOptimizer$new(
  opt("random_search", batch_size = 100),
  trm("evals", n_evals = 100)
)
bayesopt_soo(instance, acqfun, acqopt)
plot(y~batch_nr, instance$archive$data[batch_nr>1,], type = "b")
```

## Multipoint through constant liar

Currently broken example:

``` r
instance = OptimInstanceSingleCrit$new(
  objective = obfun,
  terminator = terminator
)
design = generate_design_lhs(obfun$domain, 4)$data
instance$eval_batch(design)

surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
acqfun = AcqFunctionEI$new(surrogate = surrogate)
acqopt = AcqOptimizer$new(
  opt("random_search", batch_size = 100),
  trm("evals", n_evals = 100)
)
bayesopt_mpcl(instance, acqfun, acqopt, mean, 2)
plot(y~batch_nr, instance$archive$data[batch_nr>1,], type = "b")
```
