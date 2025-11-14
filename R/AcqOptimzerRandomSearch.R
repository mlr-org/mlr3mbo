#' @title Random Search Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R mlr_acqoptimizers.R
#'
#' @description
#' Random search acquisition function optimizer.
#' By default, it samples `100 * D^2` random points in the search space, where `D` is the dimension of the search space.
#' The point with the highest acquisition value is returned.
#'
#' @section Parameters:
#' \describe{
#' \item{`n_evals`}{`integer(1)`\cr
#'   Number of random points to sample.
#'   Default is `100 * D^2`, where `D` is the dimension of the search space.}
#' }
#' @export
AcqOptimizerRandomSearch = R6Class("AcqOptimizerRandomSearch",
  inherit = AcqOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      param_set = ps(
        n_evals = p_int(lower = 1),
        catch_errors = p_lgl(init = TRUE)
      )
      private$.param_set = param_set
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      pv = self$param_set$values

      fun = get_private(self$acq_function)$.fun
      constants = self$acq_function$constants$values
      direction = self$acq_function$codomain$direction

      if (is.null(pv$n_evals)) {
        pv$n_evals = 100 * self$acq_function$domain$length^2
      }


      xdt = generate_design_random(self$acq_function$domain, n = pv$n_evals)$data

      optimize = function() {
        mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
      }

      if (pv$catch_errors) {
        tryCatch({
          ys = optimize()
        }, error = function(error_condition) {
          lg$warn(error_condition$message)
          stop(set_class(list(message = error_condition$message, call = NULL), classes = c("acq_optimizer_error", "mbo_error", "error", "condition")))
        })
      } else {
        ys = optimize()
      }

      id = if (direction == 1) which.min(ys) else which.max(ys)
      x = xdt[id, ]
      y = ys[id]

      set(x, j = self$acq_function$codomain$ids(), value = y)
      x
    }
  ),

  active = list(
    #' @template field_print_id
    print_id = function(rhs) {
      assert_ro_binding(rhs)
      "(OptimizerRandomSearch)"
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

mlr_acqoptimizers$add("random_search", AcqOptimizerRandomSearch)
