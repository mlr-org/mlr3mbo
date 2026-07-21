#' @title L-BFGS-B Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R mlr_acqoptimizers.R
#'
#' @description
#' L-BFGS-B acquisition function optimizer.
#' Calls `nloptr()` from \CRANpkg{nloptr}.
#' In its default setting, the algorithm restarts `5 * D` times and runs at most for `100 * D^2` function evaluations,
#' where `D` is the dimension of the search space.
#' Each run stops when the relative tolerance of the parameters is less than `10^-4`.
#' The first iteration starts with the best point in the archive and the next iterations start from a random point.
#'
#' Only fully numeric search spaces (all parameters of type `p_dbl`) are supported.
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
#' \item{`skip_already_evaluated`}{`logical(1)`\cr
#'   Should the proposed candidate be rejected if it was already evaluated on the actual [bbotk::OptimInstance]?
#'   If `TRUE` and the candidate was already evaluated, an error is raised so that the `loop_function` can
#'   propose a randomly sampled point instead.
#'   Default is `TRUE`.}
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
#'   Deactivate with `-1` (Default).}
#' \item{`ftol_abs`}{`numeric(1)`\cr
#'   Absolute tolerance of the objective function.
#'   Deactivate with `-1` (Default).}
#' }
#'
#' @examples
#' if (requireNamespace("nloptr")) {
#'   acqo("lbfgsb")
#' }
#' @export
AcqOptimizerLbfgsb = R6Class(
  "AcqOptimizerLbfgsb",
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
        maxeval = p_int(lower = 1, special_vals = list(-1L, -1)),
        stopval = p_dbl(default = -Inf, lower = -Inf, upper = Inf),
        xtol_rel = p_dbl(default = 1e-04, lower = 0, upper = Inf, special_vals = list(-1)),
        xtol_abs = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1)),
        ftol_rel = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1)),
        ftol_abs = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1)),
        restart_strategy = p_fct(levels = c("none", "random"), init = "none"),
        max_restarts = p_int(lower = 0L),
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
      if (!all(self$acq_function$domain$class == "ParamDbl")) {
        stopf("`AcqOptimizerLbfgsb` only supports fully numeric (`p_dbl`) search spaces.")
      }
      self$state = NULL
      pv = self$param_set$values
      restart_strategy = pv$restart_strategy
      max_restarts = pv$max_restarts
      maxeval = pv$maxeval
      catch_errors = pv$catch_errors
      skip_already_evaluated = pv$skip_already_evaluated
      pv$max_restarts = NULL
      pv$restart_strategy = NULL
      pv$maxeval = NULL
      pv$catch_errors = NULL
      pv$skip_already_evaluated = NULL

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

      # nloptr requires x0 to lie strictly within the bounds, so we shrink the box by safeguard_epsilon on each side.
      # Degenerate dimensions (span smaller than 2 * safeguard_epsilon) are collapsed to their midpoint to keep lb <= ub.
      safeguard_epsilon = 1e-5
      lower = self$acq_function$domain$lower
      upper = self$acq_function$domain$upper
      lb = lower + safeguard_epsilon
      ub = upper - safeguard_epsilon
      degenerate = lb > ub
      if (any(degenerate)) {
        mid = (lower + upper) / 2
        lb[degenerate] = mid[degenerate]
        ub[degenerate] = mid[degenerate]
      }

      y = Inf
      x = NULL
      n_evals = 0L
      n_restarts = 0L
      while ((n_evals < maxeval || maxeval < 0) && n_restarts <= max_restarts) {
        n_restarts = n_restarts + 1L

        x0 = if (n_restarts == 1L) {
          as.numeric(self$acq_function$archive$best()[, self$acq_function$domain$ids(), with = FALSE])
        } else {
          # random restart
          as.numeric(generate_design_random(self$acq_function$domain, n = 1)$data)
        }
        # clip the starting point into the shrunk box, otherwise nloptr aborts when the incumbent lies on a bound
        x0 = pmin(pmax(x0, lb), ub)

        eval_grad_f = function(x, fun, constants, direction) {
          invoke(nloptr::nl.grad, x0 = x, fn = wrapper, fun = fun, constants = constants, direction = direction)
        }

        optimize = function() {
          invoke(
            nloptr::nloptr,
            eval_f = wrapper,
            lb = lb,
            ub = ub,
            opts = insert_named(pv, list(algorithm = "NLOPT_LD_LBFGS", maxeval = maxeval - n_evals)),
            eval_grad_f = eval_grad_f,
            x0 = x0,
            fun = fun,
            constants = constants,
            direction = direction
          )
        }

        if (catch_errors) {
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

        # isTRUE guards against a NaN objective (e.g., constant acquisition surface) that would otherwise error here
        if (isTRUE(res$objective < y)) {
          y = res$objective
          x = res$solution
        }

        n_evals = n_evals + res$iterations

        self$state = c(self$state, set_names(list(list(model = res, start = x0)), paste0("iteration_", n_restarts)))

        if (restart_strategy == "none") break
      }
      if (is.null(x)) {
        error_acq_optimizer("Acquisition function optimization did not yield a valid solution.")
      }
      xdt = as.data.table(as.list(set_names(
        c(x, y * direction),
        c(self$acq_function$domain$ids(), self$acq_function$codomain$ids())
      )))
      if (skip_already_evaluated) {
        assert_not_already_evaluated(xdt, self$acq_function$archive)
      }
      xdt
    },

    #' @description
    #' Reset the acquisition function optimizer.
    #'
    #' Clears the `state` of the previous optimization run.
    reset = function() {
      self$state = NULL
    }
  ),

  active = list(
    #' @template field_print_id
    print_id = function(rhs) {
      assert_ro_binding(rhs)
      "(OptimizerLbfgsb)"
    },

    #' @template field_label
    label = function(rhs) {
      assert_ro_binding(rhs)
      "L-BFGS-B"
    },

    #' @template field_man
    man = function(rhs) {
      assert_ro_binding(rhs)
      "mlr3mbo::AcqOptimizerLbfgsb"
    }
  )
)

mlr_acqoptimizers$add("lbfgsb", AcqOptimizerLbfgsb)
