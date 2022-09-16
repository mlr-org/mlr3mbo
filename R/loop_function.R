#' @title Loop Functions for Bayesian Optimization
#'
#' @include mlr_loop_functions.R
#' @name loop_function
#'
#' @description
#' Loop functions determine the behavior of the BO algorithm on a global level.
#' For an overview of readily available loop functions, see `as.data.table(mlr_loop_functions)`.
#'
#' In general, a loop function is simply a decorated member of the S3 class `loop_function`.
#' Attributes should include: `id` (id of the loop function), `label` (brief description), `instance` ("single-crit" and
#' or "multi_crit"), and `man` (link to the manual page).
#'
#' @param instance ([bbotk::OptimInstance])\cr
#'   The [bbotk::OptimInstance] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#' @param surrogate (`NULL` | [Surrogate])\cr
#'   [Surrogate] to be used as a surrogate.
#' @param acq_function (`NULL` | [AcqFunction]).\cr
#'   [AcqFunction] to be used as acquisition function.
#' @param acq_optimizer (`NULL` | [AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#' @family Loop Function
NULL

#' @export
print.loop_function = function(x, ...) {
  catn("Loop function: ", attr(x, "id"))
  catn(str_indent("* Description:", attr(x, "label")))
  catn(str_indent("* Supported Instance:", attr(x, "instance")))
}

