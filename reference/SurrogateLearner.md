# Surrogate Model Containing a Single Learner

Surrogate model containing a single
[mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html).

## Parameters

- `catch_errors`:

  `logical(1)`  
  Should errors during updating the surrogate be caught and propagated
  to the `loop_function` which can then handle the failed acquisition
  function optimization (as a result of the failed surrogate)
  appropriately by, e.g., proposing a randomly sampled point for
  evaluation? Default is `TRUE`.

- `impute_method`:

  `character(1)`  
  Method to impute missing values in the case of updating on an
  asynchronous
  [bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
  with pending evaluations. Can be `"mean"` to use mean imputation or
  `"random"` to sample values uniformly at random between the empirical
  minimum and maximum. Default is `"random"`.

## Super class

[`mlr3mbo::Surrogate`](https://mlr3mbo.mlr-org.com/reference/Surrogate.md)
-\> `SurrogateLearner`

## Public fields

- `learner`:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  wrapped as a surrogate model.

- `input_trafo`:

  ([InputTrafo](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md))  
  Input transformation.

- `output_trafo`:

  ([OutputTrafo](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md))  
  Output transformation.

## Active bindings

- `print_id`:

  (`character`)  
  Id used when printing.

- `n_learner`:

  (`integer(1)`)  
  Returns the number of surrogate models.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled if at least one of the
  packages is not installed, but loaded (not attached) later on-demand
  via [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores the feature types the surrogate can handle, e.g. `"logical"`,
  `"numeric"`, or `"factor"`. A complete list of candidate feature
  types, grouped by task type, is stored in
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores a set of properties/capabilities the surrogate has. A complete
  list of candidate properties, grouped by task type, is stored in
  [`mlr_reflections$learner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `predict_type`:

  (`character(1)`)  
  Retrieves the currently active predict type, e.g. `"response"`.

- `output_trafo_must_be_considered`:

  (`logical(1)`)  
  Whether a transformation has been applied to the target variable that
  has not been inverted during prediction.

## Methods

### Public methods

- [`SurrogateLearner$new()`](#method-SurrogateLearner-new)

- [`SurrogateLearner$predict()`](#method-SurrogateLearner-predict)

- [`SurrogateLearner$clone()`](#method-SurrogateLearner-clone)

Inherited methods

- [`mlr3mbo::Surrogate$format()`](https://mlr3mbo.mlr-org.com/reference/Surrogate.html#method-format)
- [`mlr3mbo::Surrogate$print()`](https://mlr3mbo.mlr-org.com/reference/Surrogate.html#method-print)
- [`mlr3mbo::Surrogate$reset()`](https://mlr3mbo.mlr-org.com/reference/Surrogate.html#method-reset)
- [`mlr3mbo::Surrogate$update()`](https://mlr3mbo.mlr-org.com/reference/Surrogate.html#method-update)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    SurrogateLearner$new(
      learner,
      input_trafo = NULL,
      output_trafo = NULL,
      archive = NULL,
      cols_x = NULL,
      col_y = NULL
    )

#### Arguments

- `learner`:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)).

- `input_trafo`:

  ([InputTrafo](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md) \|
  `NULL`).
  [InputTrafo](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md) to
  be applied.

- `output_trafo`:

  ([OutputTrafo](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md)
  \| `NULL`).
  [OutputTrafo](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md) to
  be applied.

- `archive`:

  ([bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) \|
  `NULL`)  
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html).

- `cols_x`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column id's of variables that should be used as features. By default,
  automatically inferred based on the archive.

- `col_y`:

  (`character(1)` \| `NULL`)  
  Column id of variable that should be used as a target. By default,
  automatically inferred based on the archive.

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Predict mean response and standard error.

#### Usage

    SurrogateLearner$predict(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  New data. One row per observation.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the columns `mean` and `se`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SurrogateLearner$clone(deep = FALSE)

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

  surrogate = srlrn(learner, archive = instance$archive)

  surrogate$update()

  surrogate$learner$model
}
#> 
#> Call:
#> DiceKriging::km(design = data, response = truth, covtype = "matern5_2", 
#>     nugget = 4.09039649859895e-06, optim.method = "gen", control = pv$control, 
#>     scaling = FALSE)
#> 
#> Trend  coeff.:
#>                Estimate
#>  (Intercept)    55.3437
#> 
#> Covar. type  : matern5_2 
#> Covar. coeff.:
#>                Estimate
#>     theta(x)     4.1085
#> 
#> Variance estimate: 592.1794
#> 
#> Nugget effect : 4.090396e-06
#> 
```
