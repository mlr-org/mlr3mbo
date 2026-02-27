# Syntactic Sugar Input Trafo Construction

This function complements
[mlr_input_trafos](https://mlr3mbo.mlr-org.com/reference/mlr_input_trafos.md)
with functions in the spirit of `mlr_sugar` from
[mlr3](https://CRAN.R-project.org/package=mlr3).

## Usage

``` r
it(.key, ...)
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

[InputTrafo](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md)

## Examples

``` r
it("unitcube")
#> <InputTrafoUnitcube>
```
