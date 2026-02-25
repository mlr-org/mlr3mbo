# Input Transformation Unitcube

Input transformation that performs for each numeric and integer feature
min-max scaling to `[\0, 1\]` based on the boundaries of the search
space.

\[\0, 1\]: R:%5C%5C0,%201%5C

## See also

Other Input Transformation:
[`InputTrafo`](https://mlr3mbo.mlr-org.com/dev/reference/InputTrafo.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_input_trafos.md)

## Super class

[`mlr3mbo::InputTrafo`](https://mlr3mbo.mlr-org.com/dev/reference/InputTrafo.md)
-\> `InputTrafoUnitcube`

## Active bindings

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled if at least one of the
  packages is not installed, but loaded (not attached) later on-demand
  via [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

## Methods

### Public methods

- [`InputTrafoUnitcube$new()`](#method-InputTrafoUnitcube-new)

- [`InputTrafoUnitcube$update()`](#method-InputTrafoUnitcube-update)

- [`InputTrafoUnitcube$transform()`](#method-InputTrafoUnitcube-transform)

- [`InputTrafoUnitcube$clone()`](#method-InputTrafoUnitcube-clone)

Inherited methods

- [`mlr3mbo::InputTrafo$format()`](https://mlr3mbo.mlr-org.com/dev/reference/InputTrafo.html#method-format)
- [`mlr3mbo::InputTrafo$print()`](https://mlr3mbo.mlr-org.com/dev/reference/InputTrafo.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    InputTrafoUnitcube$new()

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Learn the transformation based on observed data and update parameters in
`$state`.

#### Usage

    InputTrafoUnitcube$update(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_x`.

------------------------------------------------------------------------

### Method [`transform()`](https://rdrr.io/r/base/transform.html)

Perform the transformation.

#### Usage

    InputTrafoUnitcube$transform(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Data. One row per observation with at least columns `$cols_x`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the transformation applied to the columns `$cols_x` (if applicable)
or a subset thereof.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    InputTrafoUnitcube$clone(deep = FALSE)

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

  input_trafo = it("unitcube")

  surrogate = srlrn(learner, input_trafo = input_trafo, archive = instance$archive)

  surrogate$update()

  surrogate$input_trafo$state

  surrogate$predict(data.table(x = c(-1, 0, 1)))
}
#> $mean
#> [1] 33.16169 33.16161 33.16031
#> 
#> $se
#> [1] 6.553761 6.553761 6.553761
#> 
```
