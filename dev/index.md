# mlr3mbo

Package website: [release](https://mlr3mbo.mlr-org.com/) \|
[dev](https://mlr3mbo.mlr-org.com/dev/)

A new R6 and much more modular implementation for single- and
multi-objective Bayesian Optimization.

## Get Started

The best entry point to get familiar with `mlr3mbo` is provided via the
[Bayesian
Optimization](https://mlr3book.mlr-org.com/chapters/chapter5/advanced_tuning_methods_and_black_box_optimization.html#sec-bayesian-optimization)
chapter in the `mlr3book`.

## Design

`mlr3mbo` is built modular relying on the following
[R6](https://cran.r-project.org/package=R6) classes:

- `Surrogate`: Surrogate Model
- `AcqFunction`: Acquisition Function
- `AcqOptimizer`: Acquisition Function Optimizer

Based on these, Bayesian Optimization (BO) loops can be written, see,
e.g., `bayesopt_ego` for sequential single-objective BO.

`mlr3mbo` also provides an `OptimizerMbo` class behaving like any other
`Optimizer` from the [bbotk](https://cran.r-project.org/package=bbotk)
package as well as a `TunerMbo` class behaving like any other `Tuner`
from the [mlr3tuning](https://cran.r-project.org/package=mlr3tuning)
package.

`mlr3mbo` uses sensible defaults for the `Surrogate`, `AcqFunction`,
`AcqOptimizer`, and even the `loop_function`. See
[`?mbo_defaults`](https://mlr3mbo.mlr-org.com/dev/reference/mbo_defaults.md)
for more details.

## Simple Optimization Example

Minimize the two-dimensional Branin function via sequential BO using a
GP as surrogate and EI as acquisition function optimized via a local
serch:

``` r
library(bbotk)
library(mlr3mbo)
library(mlr3learners)
set.seed(1)

fun = function(xdt) {
  y = branin(xdt[["x1"]], xdt[["x2"]])
  data.table(y = y)
}

domain = ps(
  x1 = p_dbl(-5, 10),
  x2 = p_dbl(0, 15)
)

codomain = ps(
  y = p_dbl(tags = "minimize")
)

objective = ObjectiveRFunDt$new(
  fun = fun,
  domain = domain,
  codomain = codomain
)

instance = oi(
  objective = objective,
  terminator = trm("evals", n_evals = 25)
)

surrogate = srlrn(lrn("regr.km", control = list(trace = FALSE)))

acq_function = acqf("ei")

acq_optimizer = acqo(
  opt("local_search", n_initial_points = 10, initial_random_sample_size = 1000, neighbors_per_point = 10),
  terminator = trm("evals", n_evals = 2000)
)

optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer
)

optimizer$optimize(instance)
```

``` R
##          x1       x2  x_domain        y
##       <num>    <num>    <list>    <num>
## 1: 3.104516 2.396279 <list[2]> 0.412985
```

We can quickly visualize the contours of the objective function (on log
scale) as well as the sampling behavior of our BO run (lighter blue
colours indicating points that were evaluated in later stages of the
optimization process; the first batch is given by the initial design).

``` r
library(ggplot2)
grid = generate_design_grid(instance$search_space, resolution = 1000L)$data
grid[, y := branin(x1 = x1, x2 = x2)]

ggplot(aes(x = x1, y = x2, z = log(y)), data = grid) +
  geom_contour(colour = "black") +
  geom_point(aes(x = x1, y = x2, colour = batch_nr), data = instance$archive$data) +
  labs(x =  expression(x[1]), y = expression(x[2])) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

Note that you can also use `bb_optimize` as a shorthand instead of
constructing an optimization instance.

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

``` R
##           cp learner_param_vals  x_domain classif.ce
##        <num>             <list>    <list>      <num>
## 1: -6.188733          <list[2]> <list[1]>  0.2382812
```
