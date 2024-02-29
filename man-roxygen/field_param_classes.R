#' @field param_classes (`character()`)\cr
#'   Supported parameter classes that the optimizer can optimize.
#'   Determined based on the `surrogate` and the `acq_optimizer`.
#'   This corresponds to the values given by a [paradox::ParamSet]'s
#'   `$class` field.
