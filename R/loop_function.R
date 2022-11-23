#' @title Loop Functions for Bayesian Optimization
#'
#' @include mlr_loop_functions.R
#' @name loop_function
#'
#' @description
#' Loop functions determine the behavior of the Bayesian Optimization algorithm on a global level.
#' For an overview of readily available loop functions, see `as.data.table(mlr_loop_functions)`.
#'
#' In general, a loop function is simply a decorated member of the S3 class `loop_function`.
#' Attributes must include: `id` (id of the loop function), `label` (brief description), `instance` ("single-crit" and
#' or "multi_crit"), and `man` (link to the manual page).
#'
#' As an example, see, e.g., [bayesopt_ego].
#' @family Loop Function
NULL

#' @export
print.loop_function = function(x, ...) {
  catn("Loop function: ", attr(x, "id"))
  catn(str_indent("* Description:", attr(x, "label")))
  catn(str_indent("* Supported Instance:", attr(x, "instance")))
}

