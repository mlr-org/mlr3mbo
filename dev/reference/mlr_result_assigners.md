# Dictionary of Result Assigners

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[ResultAssigner](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md).
Each result assigner has an associated help page, see
`mlr_result_assigners_[id]`.

For a more convenient way to retrieve and construct a result assigner,
see [`ras()`](https://mlr3mbo.mlr-org.com/dev/reference/ras.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar function:
[`ras()`](https://mlr3mbo.mlr-org.com/dev/reference/ras.md)

Other Dictionary:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md),
[`mlr_acqoptimizers`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqoptimizers.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_input_trafos.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_output_trafos.md)

Other Result Assigner:
[`ResultAssigner`](https://mlr3mbo.mlr-org.com/dev/reference/ResultAssigner.md),
[`mlr_result_assigners_archive`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners_archive.md),
[`mlr_result_assigners_surrogate`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners_surrogate.md)

## Examples

``` r
library(data.table)
as.data.table(mlr_result_assigners)
#> Key: <key>
#>          key                     label                                     man
#>       <char>                    <char>                                  <char>
#> 1:   archive                   Archive   mlr3mbo::mlr_result_assigners_archive
#> 2: surrogate Mean Surrogate Prediction mlr3mbo::mlr_result_assigners_surrogate
ras("archive")
#> <ResultAssignerArchive>
```
