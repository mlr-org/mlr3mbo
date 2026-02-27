# Syntactic Sugar Surrogate Construction

This function allows to construct a
[SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
or
[SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)
in the spirit of `mlr_sugar` from
[mlr3](https://CRAN.R-project.org/package=mlr3).

If the `archive` references more than one target variable or `cols_y`
contains more than one target variable but only a single `learner` is
specified, this learner is replicated as many times as needed to build
the
[SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md).

## Usage

``` r
srlrn(
  learner,
  input_trafo = NULL,
  output_trafo = NULL,
  archive = NULL,
  cols_x = NULL,
  cols_y = NULL,
  ...
)
```

## Arguments

- learner:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  \| List of
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  that is to be used within the
  [SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
  or a list of
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  that are to be used within the
  [SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md).

- input_trafo:

  (`NULL` \|
  [InputTrafo](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md))  
  Input transformation. Can also be `NULL`.'

- output_trafo:

  (`NULL` \|
  [OutputTrafo](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md))  
  Output transformation. Can also be `NULL`.

- archive:

  (`NULL` \|
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html))  
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html) of
  the
  [bbotk::OptimInstance](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  used. Can also be `NULL`.

- cols_x:

  (`NULL` \| [`character()`](https://rdrr.io/r/base/character.html))  
  Column ids in the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html)
  that should be used as features. Can also be `NULL` in which case this
  is automatically inferred based on the archive.

- cols_y:

  (`NULL` \| [`character()`](https://rdrr.io/r/base/character.html))  
  Column id(s) in the
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html)
  that should be used as a target. If a list of
  [mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  is provided as the `learner` argument and `cols_y` is specified as
  well, as many column names as learners must be provided. Can also be
  `NULL` in which case this is automatically inferred based on the
  archive.

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to be set as parameters in
  the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html).

## Value

[SurrogateLearner](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
\|
[SurrogateLearnerCollection](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)

## Examples

``` r
library(mlr3)
srlrn(lrn("regr.featureless"), catch_errors = FALSE)
#> <SurrogateLearner>: LearnerRegrFeatureless
#> * Parameters: catch_errors=FALSE, impute_method=random
srlrn(list(lrn("regr.featureless"), lrn("regr.featureless")))
#> <SurrogateLearnerCollection>: (LearnerRegrFeatureless | LearnerRegrFeatureless)
#> * Parameters: catch_errors=TRUE, impute_method=random
```
