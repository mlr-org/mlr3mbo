#' @title Acquisition Optimizer
#'
#' @description
#' Optimizer for [AcqFunction] objects.
#'
#' @export
AcqOptimizer = R6Class("AcqOptimizer",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    optimize = function(acq_function) {
      stop("abstract")
    },

    #' @description
    #' Replaces proposed points if their Gower distance with respect to other proposed points or
    #' previously evaluated points falls below a `dist_threshold`.
    #'
    #' @param xdt [data.table::data.table]\cr
    #' Proposed points.
    #'
    #' @param previous_xdt [data.table::data.table]\cr
    #' Previous evaluated points.
    #'
    #' @param search_space [paradox::ParamSet].
    #' Search space.
    #'
    #' @return [data.table::data.table()] with the same structure as the `xdt` input.
    xdt_fix_dist = function(xdt, previous_xdt, search_space) {
      dist_threshold = self$param_set$values$dist_threshold
      # first, we check wether the Gower distance of each newly proposed point to all other newly proposed points is larger than dist_threshold
      gower_distance_new = get_gower_dist(xdt)
      check_passed_new = check_gower_dist(gower_distance_new, dist_threshold = dist_threshold)

      # second, we check wether the Gower distance of all newly proposed points to the previously evaluated ones is larger than dist_threshold
      check_passed_previous = check_gower_dist(get_gower_dist(xdt, y = previous_xdt), dist_threshold = dist_threshold)

      # if either fails we iteratively replace problematic points with randomly sampled ones until both checks pass
      if (any(!(check_passed_new & check_passed_previous))) {
        lg$info("Replacing proposed point(s) to force exploration")  # FIXME: logging?
      }

      # first, replace the ones that are too close to the previous ones
      if (any(!check_passed_previous)) {
        xdt[!check_passed_previous, ] = SamplerUnif$new(search_space)$sample(sum(!check_passed_previous))$data  # FIXME: also think about augmented lhs
      }

      # second, replace the ones that are too close to the other new ones
      # do this iteratively because the distances change once a single point is replaced and we do not want to replace too many
      if (any(!check_passed_new)) {
        for (i in seq_len(sum(!check_passed_new))) {
          xdt[arrayInd(which.min(gower_distance_new), dim(gower_distance_new))[, 1L], ]  = SamplerUnif$new(search_space)$sample(1L)$data  # FIXME: also think about augmented lhs
          gower_distance_new = get_gower_dist(xdt)
          if (all(check_gower_dist(gower_distance_new, dist_threshold = dist_threshold))) {
            break  # early exit once the distances are ok
          }
        }
      }

      xdt
    }
  ),

  active = list(

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(

    .param_set = NULL,

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)
