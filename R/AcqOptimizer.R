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
    #' #FIXME: better name
    xdt_fix_distance = function(xdt, previous_xdt, search_space) {
      # FIXME: logging?
      distance_epsilon = self$param_set$values$distance_epsilon
      # first, we check wether the Gower distance of each newly proposed point to all other newly proposed points is larger than distance_epsilon
      check_passed_new = check_gower_distance(xdt, previous_xdt = previous_xdt, distance_epsilon = distance_epsilon, compare_to_itself = TRUE)

      # second, we check wether the Gower distance of all newly proposed points to the DIRECTLY previously proposed ones is larger than distance_epsilon
      # FIXME: don't we actually want to compare to ALL previously proposed points?
      check_passed_previous = check_gower_distance(xdt, previous_xdt = previous_xdt, distance_epsilon = distance_epsilon, compare_to_itself = FALSE)

      # if either fails we iteratively replace problematic points with randomly sampled ones until both checks pass
      while (any(!(check_passed_new & check_passed_previous))) {
       lg$info("Replacing proposed point(s) to force exploration")  # FIXME: logging?

        # first, replace the ones that are too close to the previous ones
        if (any(!check_passed_previous)) {
          xdt[!check_passed_previous, ] = SamplerUnif$new(search_space)$sample(sum(!check_passed_previous))$data
        }

        # second, replace the ones that are too close to the other new ones
        # do this iteratively because the distances change once a single point is replaced and we do not want to replace too many
        if (any(!check_passed_new)) {
          for (i in which(!check_passed_new)) {
            # one of the two will always be the point itself
            # FIXME: wrap gower::gower_topn to catch some safe warnings
            to_replace = gower::gower_topn(xdt[i, ], xdt, n = 2L, nthread = 2L)$index[, 1L]  # NOTE: no parallel execution for now
            xdt[to_replace[to_replace != i], ] = SamplerUnif$new(search_space)$sample(1L)$data

            if (check_gower_distance(xdt, previous_xdt = previous_xdt, distance_epsilon = distance_epsilon, compare_to_itself = TRUE)) {
              break  # early exit once the distances are ok
            }
          }
        }
        # update both checks
        check_passed_new = check_gower_distance(xdt, previous_xdt = previous_xdt, distance_epsilon = distance_epsilon, compare_to_itself = TRUE)
        check_passed_previous = check_gower_distance(xdt, previous_xdt = previous_xdt, distance_epsilon = distance_epsilon, compare_to_itself = FALSE)
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
