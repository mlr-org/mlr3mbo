#' @title Choose final point by Surrogate Mean
#'
#' @description
#' Choose final point by best surrogate mean prediction on all evaluated points on the search space.
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit] | [bbotk::OptimInstanceMultiCrit])\cr
#'   The OptimInstance object the result should be assigned to.
#' @param optimizer_mbo ([OptimizerMbo])\cr
#'   The OptimizerMbo object that generates the final result.
#' @return NULL
#' @export
result_by_surrogate_design = function(instance, optimizer_mbo) {
  archive = instance$archive
  surrogate = optimizer_mbo$acq_function$surrogate
  xydt = archive_xy(archive)
  surrogate$update(xydt = xydt, y_cols = archive$cols_y)  # update surrogate model with new data
  preds = surrogate$predict(xydt[, archive$cols_x, with = FALSE])  # FIXME: remember to set jitter to 0.001 or something if using regr.km
  means = if (testR6(surrogate, classes = "SurrogateSingleCrit")) {
    preds$mean
  } else if (testR6(surrogate, classes = "SurrogateMultiCrit")) {
    map_dtc(preds, "mean")
  }
  archive_tmp = archive$clone(deep = TRUE)
  archive_tmp$data[, archive$cols_y := means]
  best = archive_tmp$best()
  best_y = if (test_r6(instance, classes = "OptimInstanceSingleCrit")) {
    unlist(best[, archive$cols_y, with = FALSE])
  } else if (test_r6(instance, classes = "OptimInstanceMultiCrit")) {
    best[, archive$cols_y, with = FALSE]
  }
  instance$assign_result(xdt = best[, archive$cols_x, with = FALSE], best_y)
  invisible(instance)
}

