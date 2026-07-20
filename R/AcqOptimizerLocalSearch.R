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
#' If `skip_already_evaluated` is `TRUE` (default) and the proposed candidate was already evaluated on the actual
#' [bbotk::OptimInstance], an error is raised so that the `loop_function` can propose a randomly sampled point instead.
#'
#' @export
#' @examples
#' acqo("local_search")
AcqOptimizerLocalSearch = R6Class(
  "AcqOptimizerLocalSearch",
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
        skip_already_evaluated = p_lgl(init = TRUE),
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
      control = invoke(
        bbotk::local_search_control,
        minimize = self$acq_function$codomain$direction == 1L,
        .args = pv[names(pv) %nin% c("catch_errors", "skip_already_evaluated")]
      )

      wrapper = function(xdt) {
        mlr3misc::invoke(self$acq_function$fun, xdt = xdt, .args = self$acq_function$constants$values)[[1]]
      }

      optimize = function() {
        invoke(bbotk::local_search, objective = wrapper, search_space = self$acq_function$domain, control = control)
      }

      if (pv$catch_errors) {
        tryCatch(
          {
            res = optimize()
          },
          error = function(error_condition) {
            error_acq_optimizer("Acquisition function optimization failed.", parent = error_condition)
          }
        )
      } else {
        res = optimize()
      }
      xdt = as.data.table(as.list(set_names(
        c(res$x, res$y),
        c(self$acq_function$domain$ids(), self$acq_function$codomain$ids())
      )))
      if (pv$skip_already_evaluated) {
        assert_not_already_evaluated(xdt, self$acq_function$archive)
      }
      xdt
    },

    #' @description
    #' Reset the acquisition function optimizer.
    #'
    #' Currently not used.
    reset = function() {}
  ),

  active = list(
    #' @template field_print_id
    print_id = function(rhs) {
      assert_ro_binding(rhs)
      "(OptimizerLocalSearch)"
    },

    #' @template field_label
    label = function(rhs) {
      assert_ro_binding(rhs)
      "Local Search"
    },

    #' @template field_man
    man = function(rhs) {
      assert_ro_binding(rhs)
      "mlr3mbo::AcqOptimizerLocalSearch"
    }
  ),

  private = list(
    .param_set = NULL
  )
)

mlr_acqoptimizers$add("local_search", AcqOptimizerLocalSearch)
