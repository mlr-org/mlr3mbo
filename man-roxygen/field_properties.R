#' @field properties (`character()`)\cr
#'   Set of properties of the optimizer.
#'   Must be a subset of [`bbotk_reflections$optimizer_properties`][bbotk_reflections].
#'   MBO in principle is very flexible and by default we assume that the optimizer has all properties.
#'   Still, properties can be changed here manually, depending on the `loop_function`, `surrogate`, acq_function` and `acq_optimizer` at hand.
