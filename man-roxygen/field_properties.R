#' @field properties (`character()`)\cr
#'   Set of properties of the optimizer.
#'   Must be a subset of [`bbotk_reflections$optimizer_properties`][bbotk::bbotk_reflections].
#'   MBO in principle is very flexible and by default we assume that the optimizer has all properties.
#'   When fully initialized, properties are determined based on the loop, e.g., the `loop_function`, and `surrogate`.
