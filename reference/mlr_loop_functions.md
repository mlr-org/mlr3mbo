# Dictionary of Loop Functions

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class `loop_function`. Each loop function has an
associated help page, see `mlr_loop_functions_[id]`.

Retrieves object with key `key` from the dictionary. Additional
arguments must be named and are passed to the constructor of the stored
object.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Arguments

- key:

  (`character(1)`).

- ...:

  (`any`)  
  Passed down to constructor.

## Value

Object with corresponding key.

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Other Dictionary:
[`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md),
[`mlr_acqoptimizers`](https://mlr3mbo.mlr-org.com/reference/mlr_acqoptimizers.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_input_trafos.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_output_trafos.md),
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners.md)

Other Loop Function:
[`loop_function`](https://mlr3mbo.mlr-org.com/reference/loop_function.md),
[`mlr_loop_functions_ego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md),
[`mlr_loop_functions_emo`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_emo.md),
[`mlr_loop_functions_mpcl`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_mpcl.md),
[`mlr_loop_functions_parego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_parego.md),
[`mlr_loop_functions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_smsego.md)

## Examples

``` r
library(data.table)
as.data.table(mlr_loop_functions)
#> Key: <key>
#>                key                         label    instance
#>             <char>                        <char>      <char>
#> 1:    bayesopt_ego Efficient Global Optimization single-crit
#> 2:    bayesopt_emo           Multi-Objective EGO  multi-crit
#> 3:   bayesopt_mpcl      Multipoint Constant Liar single-crit
#> 4: bayesopt_parego                        ParEGO  multi-crit
#> 5: bayesopt_smsego                       SMS-EGO  multi-crit
#>                                   man
#>                                <char>
#> 1:    mlr3mbo::mlr_loop_functions_ego
#> 2:    mlr3mbo::mlr_loop_functions_emo
#> 3:   mlr3mbo::mlr_loop_functions_mpcl
#> 4: mlr3mbo::mlr_loop_functions_parego
#> 5: mlr3mbo::mlr_loop_functions_smsego
```
