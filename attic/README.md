BayOptR
================
Jakob Richter

Kind of working skeletton\!

![Dancing Skeletton](https://i.imgur.com/JODHF99.gif)

``` r
set.seed(1)
library(R6)
library(checkmate)
devtools::load_all()
```

    ## Loading BayOptR

    ## Loading required package: paradox

    ## Loading required package: data.table

## SMBO

Basic Problem Definition:

``` r
fun = function(x) sqrt(x) * sin(x) # Alpine 02
search_space = ParamSet$new(params = list(ParamDbl$new("x", 0, 10)))
target_fun = TargetFun$new(fun, search_space)
design = generate_design_lhs(search_space, 5)
design = data.table(x = design$transpose())
design$y = target_fun$eval(design)
```

MBO Problem:

``` r
op = OptProblem$new(
  target_fun = target_fun,
  design = design,
  opt_iterator = OptIteratorBO$new(),
  opt_terminator = OptTerminatorSteps$new(5),
  surrogate_model = SurrogateModelGPfit$new(design = design),
  proposal_generator = ProposalGeneratorSingle$new(
    acq_optimizer = AcqOptimizerGenSA$new(control = list(maxit = 100)),
    acq_function = AcqFunctionCB$new(lambda = 2)
  )
)

res = bayopt(op)
res$surrogate_model$design
```

    ##          x          y
    ##     <list>      <num>
    ##  1: <list>  2.5375694
    ##  2: <list> -1.8290018
    ##  3: <list>  0.6090158
    ##  4: <list>  2.6113385
    ##  5: <list>  0.2521211
    ##  6: <list> -1.8758919
    ##  7: <list> -2.1207273
    ##  8: <list> -2.1706077
    ##  9: <list> -2.1798936
    ## 10: <list> -2.1818586

``` r
res$surrogate_model$predict(res$surrogate_model$design)
```

    ##           mean           se
    ##          <num>        <num>
    ##  1:  2.5375694 7.831234e-14
    ##  2: -1.8290018 0.000000e+00
    ##  3:  0.6090158 5.786626e-15
    ##  4:  2.6113385 0.000000e+00
    ##  5:  0.2521211 0.000000e+00
    ##  6: -1.8758919 0.000000e+00
    ##  7: -2.1207273 0.000000e+00
    ##  8: -2.1706077 0.000000e+00
    ##  9: -2.1798936 0.000000e+00
    ## 10: -2.1818586 0.000000e+00

``` r
plot(res$surrogate_model$storage$model)
curve(fun(x*10), add = TRUE, col = "green") # GP_fit does some scaling
```

![](/home/richter/gits/BayOptR/README_files/figure-gfm/smbo-1.png)<!-- -->

## Multipoint MBO

### Constant Liar

``` r
op = OptProblem$new(
  target_fun = target_fun,
  design = design,
  opt_iterator = OptIteratorBO$new(),
  opt_terminator = OptTerminatorSteps$new(5),
  surrogate_model = SurrogateModelGPfit$new(design = design),
  proposal_generator = ProposalGeneratorMultiCL$new(
    acq_optimizer = AcqOptimizerGenSA$new(control = list(maxit = 100)),
    acq_function = AcqFunctionCB$new(lambda = 2),
    n = 2,
    lie = "min"
  )
)

res = bayopt(op)
```

### Ensemble

Combining multiple `ProposalGeneratorSingle` to a Multipoint Proposal.

``` r
acq_optimizer = AcqOptimizerGenSA$new(control = list(maxit = 100))

proposal_generators = lapply(c(1, 2, 4), function(x) {
  ProposalGeneratorSingle$new(
    acq_optimizer = acq_optimizer,
    acq_function = AcqFunctionCB$new(lambda = x)
  )
})

op = OptProblem$new(
  target_fun = target_fun,
  design = design,
  opt_iterator = OptIteratorBO$new(),
  opt_terminator = OptTerminatorSteps$new(5),
  surrogate_model = SurrogateModelGPfit$new(design = design),
  proposal_generator = ProposalGeneratorMultiEnsemble$new(
      proposal_generator_singles = proposal_generators
    )
)

res = bayopt(op)
```
