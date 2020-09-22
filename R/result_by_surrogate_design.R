#' @title Choose final point by Surrogate Mean
#'
#' @description
#' Choose final point by best surrogate mean prediction on all evaluated points on the search space.
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit]|[bbotk::OptimInstanceMultiCrit])\cr
#'   The OptimInstance object the result should be assigned to
#' @param optimer_mbo [OptimizerMbo]\cr
#'   The OptimizerMbo object that generates the final result.
#' @return NULL
#' @export
result_by_surrogate_design = function(instance, optimizer_mbo) {
  archive = instance$archive
  surrogate = optimizer_mbo$acq_function$surrogate
  xydt = archive$data()
  surrogate$update(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y) #update surrogate model with new data
  preds = surrogate$predict(xydt[, archive$cols_x, with = FALSE]) # remember to set jitter to 0.001 or something if using regr.km
  best = which(rank(preds$mean * mult_max_to_min(instance$objective$codomain), ties.method = "random") == 1L)
  instance$assign_result(xdt = xydt[best, archive$cols_x, with = FALSE], unlist(xydt[best, archive$cols_y, with = FALSE]))
}