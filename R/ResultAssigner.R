#' @title Result Assigner Base Class
#'
#' @include mlr_result_assigners.R
#'
#' @description
#' Abstract result assigner class.
#'
#' A result assigner is responsible for assigning the final optimization result to the [bbotk::OptimInstance].
#' Normally, it is only used within an [OptimizerMbo].
#'
#' @family Result Assigner
#' @export
ResultAssigner = R6Class("ResultAssigner",
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
    #' Assigns the result, i.e., the final point(s) to the instance.
    #'
    #' @param instance ([bbotk::OptimInstanceSingleCrit] | [bbotk::OptimInstanceMultiCrit])\cr
    #'   The [bbotk::OptimInstance] the final result should be assigned to.
    assign_result = function(instance) {
      stop("Abstract.")
    },

    #' @description
    #' Helper for print outputs.
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
    }
  ),

  private = list(
    .label = NULL,

    .man = NULL
  )
)

