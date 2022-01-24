#' @param result_function (`function`)\cr
#'   Optional function called after the optimization terminates.
#'   Determines how the final result of the optimization is calculated.
#'   Requires arguments `inst` (the [bbotk::OptimInstance]) and `self` (the [OptimizerMbo]).
#'   See for example [result_by_surrogate_design] which is used by default if the
#'   [bbotk::OptimInstance] has the property `"noisy"` (which is the case for a
#'   [mlr3tuning::TuningInstanceSingleCrit] or [mlr3tuning::TuningInstanceMultiCrit]).
