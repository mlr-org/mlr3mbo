#' @title Choose Final Point(s) based on the Archive
#'
#' @description
#' Choose final point(s) based on all evaluations in the archive.
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit] | [bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstance] the result should be assigned to.
#' @param optimizer_mbo ([OptimizerMbo])\cr
#'   The [OptimizerMbo] that generates the final result.
#' @return NULL
#' @export
result_by_default = function(instance, optimizer_mbo) {
  res = instance$archive$best()
  xdt = res[, instance$search_space$ids(), with = FALSE]
  if (inherits(instance, "OptimInstanceMultiCrit")) {
    ydt = res[, instance$archive$cols_y, with = FALSE]
    instance$assign_result(xdt, ydt)
  }
  else {
    y = unlist(res[, instance$archive$cols_y, with = FALSE])
    instance$assign_result(xdt, y)
  }
  invisible(instance)
}

