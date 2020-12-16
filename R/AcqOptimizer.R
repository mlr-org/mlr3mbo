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
    #' #FIXME:
    xdt_fix_distance = function(xdt, previous_xdt, search_space) {
      # FIXME: do we want different distances based on the nature of the search_space?
      # FIXME: wrap the gower + check part in a helper
      gower_distance = map(transpose_list(xdt), function(x) {
        gower::gower_dist(previous_xdt, setDT(x), nthread = 1L)
      })
      check_passed = map_lgl(gower_distance, function(x) all(x > self$param_set$values$distance_epsilon))  # FIXME: this is not numerically stable

      # FIXME: a while loop may be too strict here?
      while(any(!check_passed)) {
        gower_distance = map(transpose_list(xdt), function(x) {
          gower::gower_dist(previous_xdt, setDT(x), nthread = 1L)
        })
        check_passed = map_lgl(gower_distance, function(x) all(x > self$param_set$values$distance_epsilon))  # FIXME: this is not numerically stable
        xdt[!check_passed, ] = SamplerUnif$new(search_space)$sample(sum(!check_passed))$data # FIXME: logging?
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
