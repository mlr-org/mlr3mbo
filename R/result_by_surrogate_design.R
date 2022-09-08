#' @title Choose Final Point(s) by Surrogate Mean
#'
#' @description
#' Choose final point(s) by best surrogate mean prediction on all evaluated points.
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
  } else if (testR6(surrogate, classes = "SurrogateLearnerCollection")) {
    map_dtc(preds, "mean")
  }
  archive_tmp = archive$clone(deep = TRUE)
  archive_tmp$data[, surrogate$y_cols := means]
  best = archive_tmp$best()[, archive_tmp$cols_x, with = FALSE]

  # ys are still the ones originally evaluated
  best_y = if (test_r6(instance, classes = "OptimInstanceSingleCrit")) {
    unlist(archive$data[best, on = archive$cols_x][, archive$cols_y, with = FALSE])
  } else if (test_r6(instance, classes = "OptimInstanceMultiCrit")) {
    archive$data[best, on = archive$cols_x][, archive$cols_y, with = FALSE]
  }
  instance$assign_result(xdt = best, best_y)
  invisible(instance)
}

