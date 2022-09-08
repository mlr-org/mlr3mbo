#' @field packages (`character()`)\cr
#'   Set of required packages.
#'   A warning is signaled prior to optimization if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
#'   Required packages are determined based on the `surrogate` and the `acq_optimizer`.
