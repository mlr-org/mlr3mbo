# Surrogate Model Containing Multiple Learners

Surrogate model containing multiple
[mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html).
The
[mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
are fit on the target variables as indicated via `cols_y`. Note that
redundant
[mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
must be deep clones.

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
-\> `SurrogateLearnerCollection`

## Public fields

- `learner`:

  (list of
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  List of
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  wrapped as surrogate models.

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

- [`SurrogateLearnerCollection$new()`](#method-SurrogateLearnerCollection-new)

- [`SurrogateLearnerCollection$predict()`](#method-SurrogateLearnerCollection-predict)

- [`SurrogateLearnerCollection$clone()`](#method-SurrogateLearnerCollection-clone)

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

    SurrogateLearnerCollection$new(
      learners,
      input_trafo = NULL,
      output_trafo = NULL,
      archive = NULL,
      cols_x = NULL,
      cols_y = NULL
    )

#### Arguments

- `learners`:

  (list of
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)).

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

- `cols_y`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Column id's of variables that should be used as targets. By default,
  automatically inferred based on the archive.

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Predict mean response and standard error. Returns a named list of
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
Each contains the mean response and standard error for one `col_y`.

#### Usage

    SurrogateLearnerCollection$predict(xdt)

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  New data. One row per observation.

#### Returns

named list of
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the columns `mean` and `se`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SurrogateLearnerCollection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud") &
    requireNamespace("ranger")) {
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

  xdt = generate_design_random(instance$search_space, n = 4)$data

  instance$eval_batch(xdt)

  learner1 = default_gp()

  learner2 = default_rf()

  surrogate = srlrn(list(learner1, learner2), archive = instance$archive)

  surrogate$update()

  surrogate$learner

  surrogate$learner[["y1"]]$model

  surrogate$learner[["y2"]]$model
}
#> Loading required namespace: ranger
#> $model
#> Ranger result
#> 
#> Call:
#>  ranger::ranger(dependent.variable.name = task$target_names, data = data,      keep.inbag = TRUE, min.bucket = 3L, min.node.size = 3L, num.threads = 1L,      num.trees = 500L, sample.fraction = 1, splitrule = "variance",      mtry = 1) 
#> 
#> Type:                             Regression 
#> Number of trees:                  500 
#> Sample size:                      4 
#> Number of independent variables:  1 
#> Mtry:                             1 
#> Target node size:                 3 
#> Variable importance mode:         none 
#> Splitrule:                        variance 
#> OOB prediction error (MSE):       50.63955 
#> R squared (OOB):                  -0.3238282 
#> 
#> $mu_sigma
#> $mu_sigma[[1]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[2]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[3]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[4]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[5]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[6]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[7]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[8]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[9]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[10]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[11]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[12]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[13]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[14]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[15]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[16]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[17]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[18]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[19]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[20]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[21]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[22]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[23]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[24]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[25]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[26]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[27]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[28]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[29]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[30]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[31]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[32]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[33]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[34]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[35]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[36]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[37]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[38]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[39]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[40]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[41]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[42]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[43]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[44]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[45]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[46]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[47]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[48]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[49]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[50]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[51]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[52]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[53]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[54]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[55]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[56]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[57]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[58]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[59]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[60]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[61]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[62]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[63]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[64]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[65]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[66]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[67]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[68]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[69]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[70]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[71]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[72]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[73]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[74]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[75]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[76]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[77]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[78]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[79]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[80]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[81]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[82]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[83]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[84]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[85]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[86]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[87]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[88]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[89]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[90]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[91]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[92]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[93]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[94]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[95]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[96]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[97]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[98]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[99]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[100]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[101]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[102]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[103]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[104]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[105]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[106]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[107]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[108]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[109]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[110]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[111]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[112]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[113]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[114]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[115]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[116]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[117]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[118]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[119]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[120]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[121]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[122]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[123]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[124]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[125]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[126]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[127]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[128]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[129]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[130]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[131]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[132]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[133]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[134]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[135]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[136]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[137]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[138]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[139]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[140]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[141]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[142]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[143]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[144]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[145]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[146]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[147]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[148]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[149]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[150]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[151]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[152]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[153]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[154]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[155]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[156]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[157]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[158]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[159]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[160]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[161]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[162]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[163]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[164]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[165]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[166]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[167]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[168]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[169]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[170]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[171]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[172]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[173]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[174]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[175]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[176]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[177]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[178]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[179]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[180]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[181]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[182]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[183]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[184]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[185]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[186]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[187]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[188]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[189]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[190]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[191]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[192]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[193]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[194]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[195]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[196]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[197]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[198]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[199]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[200]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[201]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[202]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[203]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[204]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[205]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[206]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[207]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[208]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[209]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[210]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[211]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[212]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[213]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[214]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[215]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[216]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[217]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[218]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[219]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[220]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[221]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[222]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[223]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[224]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[225]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[226]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[227]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[228]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[229]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[230]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[231]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[232]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[233]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[234]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[235]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[236]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[237]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[238]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[239]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[240]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[241]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[242]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[243]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[244]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[245]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[246]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[247]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[248]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[249]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[250]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[251]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[252]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[253]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[254]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[255]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[256]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[257]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[258]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[259]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[260]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[261]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[262]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[263]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[264]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[265]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[266]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[267]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[268]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[269]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[270]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[271]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[272]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[273]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[274]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[275]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[276]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[277]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[278]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[279]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[280]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[281]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[282]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[283]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[284]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[285]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[286]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[287]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[288]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[289]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[290]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[291]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[292]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[293]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[294]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[295]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[296]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[297]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[298]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[299]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[300]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[301]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[302]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[303]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[304]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[305]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[306]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[307]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[308]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[309]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[310]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[311]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[312]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[313]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[314]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[315]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[316]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[317]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[318]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[319]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[320]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[321]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[322]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[323]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[324]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[325]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[326]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[327]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[328]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[329]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[330]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[331]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[332]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[333]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[334]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[335]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[336]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[337]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[338]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[339]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[340]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[341]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[342]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[343]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[344]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[345]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[346]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[347]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[348]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[349]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[350]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[351]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[352]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[353]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[354]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[355]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[356]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[357]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[358]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[359]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[360]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[361]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[362]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[363]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[364]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[365]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[366]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[367]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[368]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[369]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[370]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[371]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[372]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[373]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[374]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[375]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[376]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[377]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[378]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[379]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[380]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[381]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[382]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[383]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[384]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[385]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[386]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[387]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[388]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[389]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[390]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[391]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[392]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[393]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[394]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[395]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[396]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[397]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[398]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[399]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[400]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[401]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[402]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[403]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[404]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[405]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[406]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[407]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[408]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[409]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[410]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[411]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[412]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[413]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[414]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[415]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[416]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[417]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[418]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[419]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[420]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[421]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[422]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[423]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[424]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[425]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[426]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[427]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[428]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[429]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[430]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[431]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[432]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[433]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[434]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[435]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[436]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[437]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[438]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[439]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[440]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[441]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[442]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[443]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[444]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[445]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[446]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[447]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[448]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[449]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[450]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[451]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[452]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[453]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[454]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[455]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[456]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[457]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[458]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[459]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[460]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[461]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[462]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[463]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[464]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[465]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[466]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[467]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[468]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[469]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[470]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[471]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[472]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[473]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[474]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[475]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[476]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[477]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[478]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[479]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[480]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[481]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[482]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[483]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[484]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[485]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[486]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[487]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[488]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[489]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[490]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[491]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[492]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[493]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[494]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[495]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[496]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[497]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[498]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[499]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> $mu_sigma[[500]]
#>            mu   sigma2
#> [1,] 4.031222 38.25236
#> 
#> 
```
