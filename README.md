
# mlr3mbo

A new R6 and much more modular implementation for single- and multicrit
Bayesian optimization.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3mbo/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3mbo/actions)
[![CRANstatus](https://www.r-pkg.org/badges/version/mlr3mbo)](https://cran.r-project.org/package=mlr3mbo)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

## Simple Example

Minimize `y = x^2` via sequential BO using a GP as surrogate and EI
optimized via random search as aquisition function:

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
  id = "xsq"
)

terminator = trm("evals", n_evals = 10)

instance = OptimInstanceSingleCrit$new(
  objective = obfun,
  terminator = terminator
)

design = generate_design_lhs(obfun$domain, 4)$data
instance$eval_batch(design)

surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km", control = list(trace = FALSE)))
acqfun = AcqFunctionEI$new(surrogate = surrogate)
acqopt = AcqOptimizer$new(
  opt("random_search", batch_size = 100),
  trm("evals", n_evals = 100)
)
bayesopt_soo(instance, acqfun, acqopt)
```

    ## <Archive>
    ##          x       y           timestamp batch_nr  acq_ei
    ##  1:  1.652 2.7e+00 2021-09-29 11:35:25        1      NA
    ##  2:  4.073 1.7e+01 2021-09-29 11:35:25        1      NA
    ##  3: -4.846 2.3e+01 2021-09-29 11:35:25        1      NA
    ##  4: -1.985 3.9e+00 2021-09-29 11:35:25        1      NA
    ##  5:  1.471 2.2e+00 2021-09-29 11:35:26        2 1.4e+00
    ##  6: -0.227 5.1e-02 2021-09-29 11:35:26        3 2.4e+00
    ##  7:  0.023 5.1e-04 2021-09-29 11:35:26        4 4.3e-02
    ##  8: -0.033 1.1e-03 2021-09-29 11:35:26        5 3.4e-04
    ##  9:  0.019 3.8e-04 2021-09-29 11:35:26        6 1.3e-04
    ## 10:  0.330 1.1e-01 2021-09-29 11:35:26        7 2.0e-68
