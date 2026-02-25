#' @title Error Classes for mlr3mbo
#'
#' @description
#' Condition classes for mlr3mbo.
#'
#' @param msg (`character(1)`)\cr
#'   Error message.
#' @param ... (any)\cr
#'   Passed to [sprintf()].
#' @param class (`character`)\cr
#'   Additional class(es).
#' @param signal (`logical(1)`)\cr
#'   If `FALSE`, the condition object is returned instead of being signaled.
#'
#' @section Errors:
#' * `error_random_interleave()` for the `Mlr3ErrorMboRandomInterleave` class,
#'   signalling a random interleave error.
#' * `error_surrogate_update()` for the `Mlr3ErrorMboSurrogateUpdate` class,
#'   signalling a surrogate update error.
#' * `error_acq_optimizer()` for the `Mlr3ErrorMboAcqOptimizer` class,
#'   signalling an acquisition function optimizer error.
#'
#' @export
#' @name mlr3mbo_conditions
error_random_interleave = function(msg, ..., class = NULL, signal = TRUE) {
  error_mlr3(msg, ..., class = c(class, "Mlr3ErrorMboRandomInterleave", "Mlr3ErrorMbo"), signal = signal)
}

#' @export
#' @rdname mlr3mbo_conditions
error_surrogate_update = function(msg, ..., class = NULL, signal = TRUE) {
  error_mlr3(msg, ..., class = c(class, "Mlr3ErrorMboSurrogateUpdate", "Mlr3ErrorMbo"), signal = signal)
}

#' @export
#' @rdname mlr3mbo_conditions
error_acq_optimizer = function(msg, ..., class = NULL, signal = TRUE) {
  error_mlr3(msg, ..., class = c(class, "Mlr3ErrorMboAcqOptimizer", "Mlr3ErrorMbo"), signal = signal)
}
