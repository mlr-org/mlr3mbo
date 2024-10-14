#' @title Result Assigner Based on the Archive
#'
#' @include ResultAssigner.R
#' @name mlr_result_assigners_archive
#'
#' @description
#' Result assigner that chooses the final point(s) based on all evaluations in the [bbotk::Archive].
#' This mimics the default behavior of any [bbotk::Optimizer].
#'
#' @family Result Assigner
#' @export
#' @examples
#' result_assigner = ras("archive")
ResultAssignerArchive = R6Class("ResultAssignerArchive",
  inherit = ResultAssigner,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    initialize = function() {
      super$initialize(label = "Archive", man = "mlr3mbo::mlr_result_assigners_archive")
    },

    #' @description
    #' Assigns the result, i.e., the final point(s) to the instance.
    #'
    #' @param instance ([bbotk::OptimInstanceBatchSingleCrit] | [bbotk::OptimInstanceBatchMultiCrit])\cr
    #'   The [bbotk::OptimInstance] the final result should be assigned to.
    assign_result = function(instance) {
      xydt = instance$archive$best()
      xdt = xydt[, instance$search_space$ids(), with = FALSE]
      if (inherits(instance, "OptimInstanceBatchMultiCrit")) {
        ydt = xydt[, instance$archive$cols_y, with = FALSE]
        instance$assign_result(xdt, ydt, xydt = xydt)
      }
      else {
        y = unlist(xydt[, instance$archive$cols_y, with = FALSE])
        instance$assign_result(xdt, y, xydt = xydt)
      }
    }
  ),

  active = list(
    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
    packages = function(rhs) {
      if (missing(rhs)) {
        character(0)
      } else {
        stop("$packages is read-only.")
      }
    }
  )
)

mlr_result_assigners$add("archive", ResultAssignerArchive)

