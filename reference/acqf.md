# Syntactic Sugar Acquisition Function Construction

This function complements
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
with functions in the spirit of `mlr_sugar` from
[mlr3](https://CRAN.R-project.org/package=mlr3).

## Usage

``` r
acqf(.key, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve the object.

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to be set as parameters in
  the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html),
  or to be set as public field. See
  [`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for more details.

## Value

[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)

## Examples

``` r
acqf("ei")
#> 
#> ── <AcqFunctionEI> ─────────────────────────────────────────────────────────────
#> • Domain:
#> Empty data.table (0 rows and 5 cols): id,class,lower,upper,nlevels
#> • Codomain:
#> Empty data.table (0 rows and 4 cols): id,class,lower,upper
#> • Constants:
#>         id    class lower upper nlevels
#>     <char>   <char> <num> <num>   <num>
#> 1: epsilon ParamDbl     0   Inf     Inf
```
