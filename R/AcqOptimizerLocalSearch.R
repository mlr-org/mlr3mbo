#' @title Local Search Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R mlr_acqoptimizers.R
#'
#' @description
#' Local search acquisition function optimizer.
#' Calls [bbotk::local_search()].
#' For the meaning of the control parameters, see [bbotk::local_search_control()].
#' The termination stops when the budget defined by the `n_searches`, `n_steps`, and `n_neighs` parameters is exhausted.
#'
#' @export
#' @examples
#' acqo("local_search")
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
        stagnate_max = p_int(lower = 1L, default = 10L),
        catch_errors = p_lgl(init = TRUE)
      )
      private$.param_set = param_set
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      pv = self$param_set$get_values()
      control = invoke(bbotk::local_search_control, minimize = self$acq_function$codomain$direction == 1L, .args = pv[names(pv) != "catch_errors"])

      wrapper = function(xdt) {
        mlr3misc::invoke(self$acq_function$fun, xdt = xdt, .args = self$acq_function$constants$values)[[1]]
      }

      optimize = function() {
        invoke(bbotk::local_search,
          objective = wrapper,
          search_space = self$acq_function$domain,
          control = control)
      }

      if (pv$catch_errors) {
        tryCatch({
          res = optimize()
        }, error = function(error_condition) {
          lg$warn("Acquisition function optimization failed.")
          error_acq_optimizer("Acquisition function optimization failed.", parent = error_condition)
        })
      } else {
        res = optimize()
      }
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
      assert_ro_binding(rhs)
      "(OptimizerLocalSearch)"
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

mlr_acqoptimizers$add("local_search", AcqOptimizerLocalSearch)
