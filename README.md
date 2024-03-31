
# mlr3mbo

Package website: [release](https://mlr3mbo.mlr-org.com/) \|
[dev](https://mlr3mbo.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3mbo/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3mbo/actions/workflows/r-cmd-check.yml)
[![CRANstatus](https://www.r-pkg.org/badges/version/mlr3mbo)](https://cran.r-project.org/package=mlr3mbo)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

A new R6 and much more modular implementation for single- and
multi-objective Bayesian Optimization.

## Get Started

An overview and gentle introduction is given in [this
vignette](https://mlr3mbo.mlr-org.com/dev/articles/mlr3mbo.html).

## Design

`mlr3mbo` is built modular relying on the following
[R6](https://cran.r-project.org/package=R6) classes:

- `Surrogate`: Surrogate Model
- `AcqFunction`: Acquisition Function
- `AcqOptimizer`: Acquisition Function Optimizer

Based on these, Bayesian Optimization loops can be written, see, e.g.,
`bayesopt_ego` for sequential single-objective BO.

`mlr3mbo` also provides an `OptimizerMbo` class behaving like any other
`Optimizer` from the [bbotk](https://cran.r-project.org/package=bbotk)
package as well as a `TunerMbo` class behaving like any other `Tuner`
from the [mlr3tuning](https://cran.r-project.org/package=mlr3tuning)
package.

`mlr3mbo` uses sensible defaults for the `Surrogate`, `AcqFunction`,
`AcqOptimizer`, and even the `loop_function`. See `?mbo_defaults` for
more details.

## Simple Optimization Example

Minimize `f(x) = x^2` via sequential single-objective BO using a GP as
surrogate and EI optimized via random search as acquisition function:

``` r
library(bbotk)
library(mlr3mbo)
library(mlr3learners)
set.seed(1)

obfun = ObjectiveRFun$new(
  fun = function(xs) list(y1 = xs$x ^ 2),
  domain = ps(x = p_dbl(lower = -10, upper = 10)),
  codomain = ps(y1 = p_dbl(tags = "minimize")))

instance = OptimInstanceSingleCrit$new(
  objective = obfun,
  terminator = trm("evals", n_evals = 10))

surrogate = srlrn(lrn("regr.km", control = list(trace = FALSE)))
acqfun = acqf("ei")
acqopt = acqo(opt("random_search", batch_size = 100),
  terminator = trm("evals", n_evals = 100))

optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acqfun,
  acq_optimizer = acqopt)

optimizer$optimize(instance)
```

    ##             x  x_domain          y1
    ## 1: 0.03897209 <list[1]> 0.001518824

Note that you can also use `bb_optimize` as a shorthand:

``` r
library(bbotk)
library(mlr3mbo)
library(mlr3learners)
set.seed(1)

fun = function(xs) list(y1 = xs$x ^ 2)

surrogate = srlrn(lrn("regr.km", control = list(trace = FALSE)))
acqfun = acqf("ei")
acqopt = acqo(opt("random_search", batch_size = 100),
  terminator = trm("evals", n_evals = 100))

optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acqfun,
  acq_optimizer = acqopt)

result = bb_optimize(
  fun,
  method = optimizer,
  lower = c(x = -10),
  upper = c(x = 10),
  max_evals = 10)
```

## Simple Tuning Example

``` r
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3mbo)
set.seed(1)

task = tsk("pima")

learner = lrn("classif.rpart", cp = to_tune(lower = 1e-04, upper = 1, logscale = TRUE))

instance = tune(
  tuner = tnr("mbo"),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10)

instance$result
```

    ##           cp learner_param_vals  x_domain classif.ce
    ## 1: -4.594102          <list[2]> <list[1]>  0.2109375
