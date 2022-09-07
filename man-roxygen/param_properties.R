#' @param properties (`character()`)\cr
#'   Set of properties of the optimizer.
#'   Must be a subset of [`bbotk_reflections$optimizer_properties`][bbotk_reflections].
#'   MBO in principle is very flexible and by default we assume that the optimizer has all properties.
#'   The user must then take care that the choice of the `loop_function`, `surrogate`, `acq_function` and `acq_optimizer` match their use case.
#'   Nevertheless, properties can also be changed manually after construction by using the `properties` active binding.
