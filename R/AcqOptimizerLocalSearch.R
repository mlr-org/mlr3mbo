#' @title Local Search Acquisition Function Optimizer
#'
#' @export
AcqOptimizerLocalSearch = R6Class("AcqOptimizerLocalSearch",
  inherit = AcqOptimizer,
  public = list(

    #' @field state (`list()`)\cr
    #' List of [cmaes::cma_es()] results.
    state = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      param_set = ps(
        n_searches = p_int(lower = 1L, default = 10L),
        n_steps = p_int(lower = 0L, default = 5L),
        n_neighs = p_int(lower = 1L, default = 10L),
        mut_sd = p_dbl(lower = 0, default = 0.1),
        stagnate_max = p_int(lower = 1L, default = 10L)
      )
      private$.param_set = param_set
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      pv = self$param_set$get_values()

      local_search_control = invoke(local_search_control, minimize = self$acq_function$direction == "minimize", .args = pv)

      wrapper = function(xdt) {
        mlr3misc::invoke(self$acq_function$fun, xdt = xdt, .args = self$acq_function$constants$values)[[1]]
      }

      res = invoke(local_search,
        objective = wrapper,
        search_space = self$acq_function$domain,
        control = local_search_control
      )

      as.data.table(as.list(set_names(c(res$x, res$y), c(self$acq_function$domain$ids(), self$acq_function$codomain$ids()))))
    },

    #' @description
    #' Reset the acquisition function optimizer.
    #'
    #' Currently not used.
    reset = function() {

    }
  ),

  active = list(
    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        paste0("(", class(self$optimizer)[1L], " | ", class(self$terminator)[1L], ")")
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #'   Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .param_set = NULL,

    deep_clone = function(name, value) {
      switch(name,
        optimizer = value$clone(deep = TRUE),
        terminator = value$clone(deep = TRUE),
        acq_function = if (!is.null(value)) value$clone(deep = TRUE) else NULL,
        .param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)

