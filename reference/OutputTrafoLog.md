# Output Transformation Log

Output transformation that takes the logarithm after min-max scaling to
`\(0, 1\)`.

## See also

Other Output Transformation:
[`OutputTrafo`](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md),
[`OutputTrafoStandardize`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoStandardize.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_output_trafos.md)

## Super class

[`mlr3mbo::OutputTrafo`](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md)
-\> `OutputTrafoLog`

## Active bindings

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled if at least one of the
  packages is not installed, but loaded (not attached) later on-demand
  via [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

## Methods

### Public methods

- [`OutputTrafoLog$new()`](#method-OutputTrafoLog-new)

- [`OutputTrafoLog$update()`](#method-OutputTrafoLog-update)

- [`OutputTrafoLog$transform()`](#method-OutputTrafoLog-transform)

- [`OutputTrafoLog$inverse_transform_posterior()`](#method-OutputTrafoLog-inverse_transform_posterior)

- [`OutputTrafoLog$inverse_transform()`](#method-OutputTrafoLog-inverse_transform)

- [`OutputTrafoLog$clone()`](#method-OutputTrafoLog-clone)

Inherited methods

- [`mlr3mbo::OutputTrafo$format()`](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.html#method-format)
- [`mlr3mbo::OutputTrafo$print()`](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OutputTrafoLog$new(invert_posterior = FALSE)

#### Arguments

- `invert_posterior`:

  (`logical(1)`)  
  Should the posterior predictive distribution be inverted when used
  within a
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
  or
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)?
  Default is `FALSE`.

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Learn the transformation based on observed data and update parameters in
`$state`.

#### Usage

    OutputTrafoLog$update(ydt)

#### Arguments

- `ydt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with columns `$cols_y`.

------------------------------------------------------------------------

### Method [`transform()`](https://rdrr.io/r/base/transform.html)

Perform the transformation.

#### Usage

    OutputTrafoLog$transform(ydt)

#### Arguments

- `ydt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_y`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the transformation applied to the columns `$cols_y`.

------------------------------------------------------------------------

### Method `inverse_transform_posterior()`

Perform the inverse transformation on a posterior predictive
distribution characterized by the first and second moment.

#### Usage

    OutputTrafoLog$inverse_transform_posterior(pred)

#### Arguments

- `pred`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation characterizing a posterior predictive
  distribution with the columns `mean` and `se`. Can also be a named
  list of
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with posterior predictive distributions for multiple targets
  corresponding to (`cols_y`).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the inverse transformation applied to the columns `mean` and `se`.
In the case of the input being a named list of
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html),
the output will be a named list of
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the inverse transformation applied to the columns `mean` and `se`.

------------------------------------------------------------------------

### Method `inverse_transform()`

Perform the inverse transformation.

#### Usage

    OutputTrafoLog$inverse_transform(ydt)

#### Arguments

- `ydt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_y`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the inverse transformation applied to the columns `$cols_y`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputTrafoLog$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
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

  xdt = generate_design_random(instance$search_space, n = 4)$data

  instance$eval_batch(xdt)

  learner = default_gp()

  output_trafo = ot("log", invert_posterior = TRUE)

  surrogate = srlrn(learner, output_trafo = output_trafo, archive = instance$archive)

  surrogate$update()

  surrogate$output_trafo$state

  surrogate$predict(data.table(x = c(-1, 0, 1)))

  surrogate$output_trafo$invert_posterior = FALSE

  surrogate$predict(data.table(x = c(-1, 0, 1)))
}
#> $mean
#> [1] -6.561901 -4.102926 -2.834136
#> 
#> $se
#> [1] 1.197953 2.551467 2.729241
#> 
```
