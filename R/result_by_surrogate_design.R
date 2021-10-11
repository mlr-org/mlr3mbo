#' @title Choose Final Point by Surrogate Mean
#'
#' @description
#' Choose final point by best surrogate mean prediction on all evaluated points.
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit] | [bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstance] the result should be assigned to.
#' @param optimizer_mbo ([OptimizerMbo])\cr
#'   The [OptimizerMbo] that generates the final result.
#' @return NULL
#' @export
result_by_surrogate_design = function(instance, optimizer_mbo) {
  archive = instance$archive
  surrogate = optimizer_mbo$acq_function$surrogate
  surrogate$archive = archive
  xdt = archive_x(archive)
  surrogate$update()
  preds = surrogate$predict(xdt)
  means = if (testR6(surrogate, classes = "SurrogateLearner")) {
    preds$mean
  } else if (testR6(surrogate, classes = "SurrogateLearners")) {
    map_dtc(preds, "mean")
  }
  archive_tmp = archive$clone(deep = TRUE)
  archive_tmp$data[, surrogate$y_cols := means]
  best = archive_tmp$best()
  best_y = if (test_r6(instance, classes = "OptimInstanceSingleCrit")) {
    unlist(best[, archive$cols_y, with = FALSE])
  } else if (test_r6(instance, classes = "OptimInstanceMultiCrit")) {
    best[, archive$cols_y, with = FALSE]
  }
  instance$assign_result(xdt = best[, archive$cols_x, with = FALSE], best_y)
  invisible(instance)
}

