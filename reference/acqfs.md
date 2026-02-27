# Syntactic Sugar Acquisition Functions Construction

This function complements
[mlr_acqfunctions](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
with functions in the spirit of `mlr_sugar` from
[mlr3](https://CRAN.R-project.org/package=mlr3).

## Usage

``` r
acqfs(.keys, ...)
```

## Arguments

- .keys:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Keys passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve multiple objects.

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named arguments passed to the constructor, to be set as parameters in
  the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html),
  or to be set as public field. See
  [`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
  for more details.

## Value

List of
[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)s

## Examples

``` r
acqfs(c("ei", "pi", "cb"))
#> $acq_ei
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
#> 
#> $acq_pi
#> 
#> ── <AcqFunctionPI> ─────────────────────────────────────────────────────────────
#> • Domain:
#> Empty data.table (0 rows and 5 cols): id,class,lower,upper,nlevels
#> • Codomain:
#> Empty data.table (0 rows and 4 cols): id,class,lower,upper
#> 
#> $acq_cb
#> 
#> ── <AcqFunctionCB> ─────────────────────────────────────────────────────────────
#> • Domain:
#> Empty data.table (0 rows and 5 cols): id,class,lower,upper,nlevels
#> • Codomain:
#> Empty data.table (0 rows and 4 cols): id,class,lower,upper
#> • Constants:
#>        id    class lower upper nlevels
#>    <char>   <char> <num> <num>   <num>
#> 1: lambda ParamDbl     0   Inf     Inf
#> 
```
