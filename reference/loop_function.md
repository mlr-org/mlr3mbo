# Loop Functions for Bayesian Optimization

Loop functions determine the behavior of the Bayesian Optimization
algorithm on a global level. For an overview of readily available loop
functions, see `as.data.table(mlr_loop_functions)`.

In general, a loop function is simply a decorated member of the S3 class
`loop_function`. Attributes must include: `id` (id of the loop
function), `label` (brief description), `instance` ("single-crit" and or
"multi_crit"), and `man` (link to the manual page).

As an example, see, e.g.,
[bayesopt_ego](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md).

## See also

Other Loop Function:
[`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md),
[`mlr_loop_functions_ego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md),
[`mlr_loop_functions_emo`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_emo.md),
[`mlr_loop_functions_mpcl`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_mpcl.md),
[`mlr_loop_functions_parego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_parego.md),
[`mlr_loop_functions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_smsego.md)
