# Dictionary of Acquisition Function Optimizers

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[AcqOptimizer](https://mlr3mbo.mlr-org.com/dev/reference/AcqOptimizer.md).
Each input transformation has an associated help page, see
`mlr_acqoptimizers[id]`.

For a more convenient way to retrieve and construct an acquisition
function optimizer, see
[`acqo()`](https://mlr3mbo.mlr-org.com/dev/reference/acqo.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar function:
[`acqo()`](https://mlr3mbo.mlr-org.com/dev/reference/acqo.md)

Other Dictionary:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_acqfunctions.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_input_trafos.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_loop_functions.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_output_trafos.md),
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/dev/reference/mlr_result_assigners.md)

## Examples

``` r
library(data.table)
as.data.table(mlr_acqoptimizers)
#> Key: <key>
#>              key         label                               man
#>           <char>        <char>                            <char>
#> 1:        direct        DIRECT       mlr3mbo::AcqOptimizerDirect
#> 2:        lbfgsb      L-BFGS-B       mlr3mbo::AcqOptimizerLbfgsb
#> 3:  local_search  Local Search  mlr3mbo::AcqOptimizerLocalSearch
#> 4: random_search Random Search mlr3mbo::AcqOptimizerRandomSearch
acqo("local_search")
#> <AcqOptimizerLocalSearch>: (OptimizerLocalSearch)
#> * Parameters: catch_errors=TRUE
```
