# Dictionary of Output Transformations

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[OutputTrafo](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md).
Each output transformation has an associated help page, see
`mlr_output_trafos[id]`.

For a more convenient way to retrieve and construct an output trafo, see
[`ot()`](https://mlr3mbo.mlr-org.com/reference/ot.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar function: [`ot()`](https://mlr3mbo.mlr-org.com/reference/ot.md)

Other Dictionary:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqoptimizers`](https://mlr3mbo.mlr-org.com/reference/mlr_acqoptimizers.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_input_trafos.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md),
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners.md)

Other Output Transformation:
[`OutputTrafo`](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md),
[`OutputTrafoLog`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoLog.md),
[`OutputTrafoStandardize`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoStandardize.md)

## Examples

``` r
library(data.table)
as.data.table(mlr_output_trafos)
#> Key: <key>
#>            key       label                                    man
#>         <char>      <char>                                 <char>
#> 1:         log         Log         mlr3mbo::mlr_output_trafos_log
#> 2: standardize Standardize mlr3mbo::mlr_output_trafos_standardize
ot("standardize")
#> <OutputTrafoStandardize>
```
