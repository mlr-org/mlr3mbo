# Dictionary of Acquisition Functions

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md).
Each acquisition function has an associated help page, see
`mlr_acqfunctions_[id]`.

For a more convenient way to retrieve and construct an acquisition
function, see [`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md)
and [`acqfs()`](https://mlr3mbo.mlr-org.com/reference/acqfs.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## See also

Sugar functions:
[`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md),
[`acqfs()`](https://mlr3mbo.mlr-org.com/reference/acqfs.md)

Other Dictionary:
[`mlr_acqoptimizers`](https://mlr3mbo.mlr-org.com/reference/mlr_acqoptimizers.md),
[`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_input_trafos.md),
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md),
[`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_output_trafos.md),
[`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners.md)

Other Acquisition Function:
[`AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md),
[`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md),
[`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md),
[`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md),
[`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md),
[`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md),
[`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei_log.md),
[`mlr_acqfunctions_eips`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_eips.md),
[`mlr_acqfunctions_mean`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_mean.md),
[`mlr_acqfunctions_multi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_multi.md),
[`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_pi.md),
[`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.md),
[`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md),
[`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md),
[`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_ei.md)

## Examples

``` r
library(data.table)
as.data.table(mlr_acqfunctions)
#> Key: <key>
#>               key                                                        label
#>            <char>                                                       <char>
#>  1:           aei                               Augmented Expected Improvement
#>  2:            cb                               Lower / Upper Confidence Bound
#>  3:          ehvi                             Expected Hypervolume Improvement
#>  4:        ehvigh           Expected Hypervolume Improvement via GH Quadrature
#>  5:            ei                                         Expected Improvement
#>  6:        ei_log                            Expected Improvement on Log Scale
#>  7:          eips                              Expected Improvement Per Second
#>  8:          mean                                               Posterior Mean
#>  9:         multi Acquisition Function Wrapping Multiple Acquisition Functions
#> 10:            pi                                   Probability Of Improvement
#> 11:            sd                                 Posterior Standard Deviation
#> 12:        smsego                                                      SMS-EGO
#> 13: stochastic_cb                    Stochastic Lower / Upper Confidence Bound
#> 14: stochastic_ei                              Stochastic Expected Improvement
#>                                         man
#>                                      <char>
#>  1:           mlr3mbo::mlr_acqfunctions_aei
#>  2:            mlr3mbo::mlr_acqfunctions_cb
#>  3:          mlr3mbo::mlr_acqfunctions_ehvi
#>  4:        mlr3mbo::mlr_acqfunctions_ehvigh
#>  5:            mlr3mbo::mlr_acqfunctions_ei
#>  6:        mlr3mbo::mlr_acqfunctions_ei_log
#>  7:          mlr3mbo::mlr_acqfunctions_eips
#>  8:          mlr3mbo::mlr_acqfunctions_mean
#>  9:         mlr3mbo::mlr_acqfunctions_multi
#> 10:            mlr3mbo::mlr_acqfunctions_pi
#> 11:            mlr3mbo::mlr_acqfunctions_sd
#> 12:        mlr3mbo::mlr_acqfunctions_smsego
#> 13: mlr3mbo::mlr_acqfunctions_stochastic_cb
#> 14: mlr3mbo::mlr_acqfunctions_stochastic_ei
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
