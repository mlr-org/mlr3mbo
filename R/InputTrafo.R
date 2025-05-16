#' @title Input Transformation Base Class
#'
#' @include mlr_input_trafos.R
#'
#' @description
#' Abstract input transformation class.
#'
#' An input transformation can be used within a [Surrogate] to perform a transformation of the feature variables.
#'
#' @family Input Transformation
#' @export
InputTrafo = R6Class("InputTrafo",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(label = NA_character_, man = NA_character_) {
      private$.label = assert_string(label, na.ok = TRUE)
      private$.man = assert_string(man, na.ok = TRUE)
    },

    #' @description
    #' Learn the transformation based on observed data and update parameters in `$state`.
    #' Must be implemented by subclasses.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_x`.
    update = function(xdt) {
      stop("Abstract.")
    },

    #' @description
    #' Perform the transformation.
    #' Must be implemented by subclasses.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_x`.
    #'
    #' @return [data.table::data.table()] with the transformation applied to the columns `$cols_x` (if applicable) or a subset thereof.
    transform = function(xdt) {
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

    #' @template field_input_trafo_state
    state = function(rhs) {
      if (missing(rhs)) {
        private$.state
      } else {
        stop("$state is read-only.")
      }
    },

    #' @field search_space ([paradox::ParamSet])\cr
    #'   Search space.
    search_space = function(rhs) {
      if (missing(rhs)) {
        private$.search_space
      } else {
        private$.search_space = assert_r6(rhs, classes = "ParamSet")
      }
    },

    #' @field cols_x ([paradox::ParamSet])\cr
    #'   Column ids of feature variables that should be transformed.
    cols_x = function(rhs) {
      if (missing(rhs)) {
        private$.cols_x
      } else {
        private$.cols_x = assert_character(rhs)
      }
    }
  ),

  private = list(
    .label = NULL,

    .man = NULL,

    .state = NULL,

    .search_space = NULL,

    .cols_x = NULL
  )
)

