#' @title Output Transformation Base Class
#'
#' @include mlr_output_trafos.R
#'
#' @description
#' Abstract output transformation class.
#'
#' An output transformation can be used within a [Surrogate] to perform a transformation of the target variable(s).
#'
#' @family Output Transformation
#' @export
OutputTrafo = R6Class("OutputTrafo",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param invert_posterior (`logical(1)`)\cr
    #'  Should the posterior predictive distribution be inverted when used within a [SurrogateLearner] or [SurrogateLearnerCollection]?
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(invert_posterior, label = NA_character_, man = NA_character_) {
      private$.invert_posterior = assert_flag(invert_posterior)
      private$.label = assert_string(label, na.ok = TRUE)
      private$.man = assert_string(man, na.ok = TRUE)
    },

    #' @description
    #' Learn the transformation based on observed data and update parameters in `$state`.
    #' Must be implemented by subclasses.
    #'
    #' @param ydt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_y`.
    update = function(ydt) {
      stop("Abstract.")
    },

    #' @description
    #' Perform the transformation.
    #' Must be implemented by subclasses.
    #'
    #' @param ydt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_y`.
    #'
    #' @return [data.table::data.table()] with the transformation applied to the columns `$cols_y`.
    transform = function(ydt) {
      stop("Abstract.")
    },

    #' @description
    #' Perform the inverse transformation on a posterior predictive distribution characterized by the first and second moment.
    #' Must be implemented by subclasses.
    #'
    #' @param pred ([data.table::data.table()])\cr
    #'   Data. One row per observation characterizing a posterior predictive distribution with the columns `mean` and `se`.
    #'
    #' @return [data.table::data.table()] with the inverse transformation applied to the columns `mean` and `se`.
    inverse_transform_posterior = function(pred) {
      stop("Abstract.")
    },

    #' @description
    #' Perform the inverse transformation.
    #' Must be implemented by subclasses.
    #'
    #' @param ydt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_y`.
    #'
    #' @return [data.table::data.table()] with the inverse transformation applied to the columns `$cols_y`.
    inverse_transform = function(ydt) {
      stop("Abstract.")
    },

    #' @description
    #' Helper for print outputs.
    #'
    #' @return (`character(1)`).
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self))
    }
  ),

  active = list(
    #' @field label (`character(1)`)\cr
    #'   Label for this object.
    label = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.label)) {
        stop("$label is read-only.")
      }
      private$.label
    },

    #' @field man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    man = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.man)) {
        stop("$man is read-only.")
      }
      private$.man
    },

    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
    packages = function(rhs) {
      if (missing(rhs)) {
        stop("Abstract.")
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_output_trafo_state
    state = function(rhs) {
      if (missing(rhs)) {
        private$.state
      } else {
        stop("$state is read-only.")
      }
    },

    #' @field cols_y (`character()` | `NULL`)\cr
    #'   Column ids of target variables that should be transformed.
    cols_y = function(rhs) {
      if (missing(rhs)) {
        private$.cols_y
      } else {
        private$.cols_y = assert_character(rhs)
      }
    },

    #' @field max_to_min (`-1` | `1`)\cr
    #'   Multiplicative factor to correct for minimization or maximization.
    max_to_min = function(rhs) {
      if (missing(rhs)) {
        private$.max_to_min
      } else {
        private$.max_to_min = assert_subset(rhs, choices = c(-1L, 1L))
      }
    },

    #' @field invert_posterior (`logical(1)`)\cr
    #'   Should the posterior predictive distribution be inverted when used within a [SurrogateLearner] or [SurrogateLearnerCollection]?
    invert_posterior = function(rhs) {
      if (missing(rhs)) {
        private$.invert_posterior
      } else {
        private$.invert_posterior = assert_flag(rhs)
      }
    }
  ),

  private = list(
    .label = NULL,

    .man = NULL,

    .state = NULL,

    .cols_y = NULL,

    .max_to_min = NULL,

    .invert_posterior = NULL
  )
)

