#' @title Direct Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R mlr_acqoptimizers.R
#'
#' @description
#' Direct acquisition function optimizer.
#' Calls `nloptr()` from \CRANpkg{nloptr}.
#' In its default setting, the algorithm restarts `5 * D` times and runs at most for `100 * D^2` function evaluations, where `D` is the dimension of the search space.
#' Each run stops when the relative tolerance of the parameters is less than `10^-4`.
#' The first iteration starts with the best point in the archive and the next iterations start from a random point.
#'
#' @section Parameters:
#' \describe{
#' \item{`restart_strategy`}{`character(1)`\cr
#'   Restart strategy.
#'   Can be `"none"` or `"random"`.
#'   Default is `"none"`.
#' }
#' \item{`max_restarts`}{`integer(1)`\cr
#'   Maximum number of restarts.
#'   Default is `5 * D` (Default).}
#' }
#'
#' @note
#' If the restart strategy is `"none"`, the optimizer starts with the best point in the archive.
#' The optimization stops when one of the stopping criteria is met.
#'
#' If `restart_strategy` is `"random"`, the optimizer runs at least for `maxeval` iterations.
#' The first iteration starts with the best point in the archive and stops when one of the stopping criteria is met.
#' The next iterations start from a random point.
#'
#' @section Termination Parameters:
#' The following termination parameters can be used.
#'
#' \describe{
#' \item{`stopval`}{`numeric(1)`\cr
#'   Stop value.
#'   Deactivate with `-Inf` (Default).}
#' \item{`maxeval`}{`integer(1)`\cr
#'   Maximum number of evaluations.
#'   Default is `100 * D^2`, where `D` is the dimension of the search space.
#'   Deactivate with `-1L`.}
#' \item{`xtol_rel`}{`numeric(1)`\cr
#'   Relative tolerance of the parameters.
#'   Default is `10^-4`.
#'   Deactivate with `-1`.}
#' \item{`xtol_abs`}{`numeric(1)`\cr
#'   Absolute tolerance of the parameters.
#'   Deactivate with `-1` (Default).}
#' \item{`ftol_rel`}{`numeric(1)`\cr
#'   Relative tolerance of the objective function.
#'   Deactivate with `-1`. (Default).}
#' \item{`ftol_abs`}{`numeric(1)`\cr
#'   Absolute tolerance of the objective function.
#'   Deactivate with `-1` (Default).}
#' }
#'
#' @export
#' @examples
#' if (requireNamespace("nloptr")) {
#'   acqo("direct")
#' }
AcqOptimizerDirect = R6Class("AcqOptimizerDirect",
  inherit = AcqOptimizer,
  public = list(

    #' @field state (`list()`)\cr
    #' List of [nloptr::nloptr()] results.
    state = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      param_set = ps(
        maxeval = p_int(),
        stopval = p_dbl(default = -Inf, lower = -Inf, upper = Inf),
        xtol_rel = p_dbl(default = 1e-04, lower = 0, upper = Inf, special_vals = list(-1L)),
        xtol_abs = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1L)),
        ftol_rel = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1L)),
        ftol_abs = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1L)),
        minf_max = p_dbl(default = -Inf),
        restart_strategy = p_fct(levels = c("none", "random"), init = "random"),
        max_restarts = p_int(lower = 0L),
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
      restart_strategy = pv$restart_strategy
      max_restarts = pv$max_restarts
      maxeval = pv$maxeval
      pv$max_restarts = NULL
      pv$restart_strategy = NULL
      pv$maxeval = NULL

      if (restart_strategy == "none") {
        max_restarts = 0L
      } else if (restart_strategy == "random" && is.null(max_restarts)) {
        max_restarts = 5 * self$acq_function$domain$length
      }

      if (is.null(maxeval)) {
        maxeval = 100 * self$acq_function$domain$length^2
      }

      wrapper = function(x, fun, constants, direction) {
        xdt = as.data.table(as.list(set_names(x, self$acq_function$domain$ids())))
        res = mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
        res * direction
      }

      fun = get_private(self$acq_function)$.fun
      constants = self$acq_function$constants$values
      direction = self$acq_function$codomain$direction

      y = Inf
      n_evals = 0L
      n_restarts = 0L
      while (n_evals < maxeval || maxeval < 0 && n_restarts <= max_restarts) {
        n_restarts = n_restarts + 1L

        x0 = if (n_restarts == 1L) {
          as.numeric(self$acq_function$archive$best()[, self$acq_function$domain$ids(), with = FALSE])
        } else {
          # random restart
          as.numeric(generate_design_random(self$acq_function$domain, n = 1)$data)
        }

        optimize = function() {
          invoke(nloptr::nloptr,
            eval_f = wrapper,
            lb = self$acq_function$domain$lower,
            ub = self$acq_function$domain$upper,
            opts = insert_named(pv, list(algorithm = "NLOPT_GN_DIRECT_L", maxeval = maxeval - n_evals)),
            eval_grad_f = NULL,
            x0 = x0,
            fun = fun,
            constants = constants,
            direction = direction)
        }

        if (pv$catch_errors) {
          tryCatch({
            res = optimize()
          }, error = function(error_condition) {
            lg$warn("Caught the following error: %s", error_condition$message)
            error_acq_optimizer("Acquisition function optimization failed.")
          })
        } else {
          res = optimize()
        }

        if (res$objective < y) {
          y = res$objective
          x = res$solution
        }

        n_evals = n_evals + res$iterations

        self$state = c(self$state, set_names(list(list(model = res, start = x0)), paste0("iteration_", n_restarts)))

        if (restart_strategy == "none") break
      }
      as.data.table(as.list(set_names(c(x, y * direction), c(self$acq_function$domain$ids(), self$acq_function$codomain$ids()))))
    }
  ),

  active = list(
    #' @template field_print_id
    print_id = function(rhs) {
      assert_ro_binding(rhs)
      "(OptimizerDirect)"
    }
  )
)

mlr_acqoptimizers$add("direct", AcqOptimizerDirect)
