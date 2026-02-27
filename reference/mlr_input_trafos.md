# Dictionary of Input Transformations

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[InputTrafo](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md). Each
input transformation has an associated help page, see
`mlr_input_trafos[id]`.

For a more convenient way to retrieve and construct an input trafo, see
[`it()`](https://mlr3mbo.mlr-org.com/reference/it.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar function: [`it()`](https://mlr3mbo.mlr-org.com/reference/it.md)

Other Dictionary:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqoptimizers`](https://mlr3mbo.mlr-org.com/reference/mlr_acqoptimizers.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_output_trafos.md),
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners.md)

Other Input Transformation:
[`InputTrafo`](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md),
[`InputTrafoUnitcube`](https://mlr3mbo.mlr-org.com/reference/InputTrafoUnitcube.md)

## Examples

``` r
library(data.table)
as.data.table(mlr_input_trafos)
#> Key: <key>
#>         key    label                                man
#>      <char>   <char>                             <char>
#> 1: unitcube Unitcube mlr3mbo::mlr_input_trafos_unitcube
it("unitcube")
#> <InputTrafoUnitcube>
```
