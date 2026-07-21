#' @title Direct Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R mlr_acqoptimizers.R
#'
#' @description
#' Direct acquisition function optimizer.
#' Calls `nloptr()` from \CRANpkg{nloptr} with the `NLOPT_GN_DIRECT_L` algorithm.
#' In its default setting, the algorithm runs for at most `100 * D^2` function evaluations,
#' where `D` is the dimension of the search space.
#' The optimization stops when the relative tolerance of the parameters is less than `10^-4`.
#'
#' Only fully numeric search spaces (all parameters of type `p_dbl`) are supported.
#'
#' @note
#' `NLOPT_GN_DIRECT_L` is a deterministic global optimizer that ignores the starting point.
#' Restarts would only repeat the identical search, so the optimizer does not support them.
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
AcqOptimizerDirect = R6Class(
  "AcqOptimizerDirect",
  inherit = AcqOptimizer,
  public = list(
    #' @field state ([nloptr::nloptr()] result)\cr
    #' Result of the last optimization run.
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
        minf_max = p_dbl(default = -Inf),
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
        stopf("`AcqOptimizerDirect` only supports fully numeric (`p_dbl`) search spaces.")
      }
      self$state = NULL
      pv = self$param_set$values
      maxeval = pv$maxeval
      catch_errors = pv$catch_errors
      pv$maxeval = NULL
      pv$catch_errors = NULL

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

      # NLOPT_GN_DIRECT_L ignores the starting point; nloptr only requires x0 to infer the dimension
      x0 = (self$acq_function$domain$lower + self$acq_function$domain$upper) / 2

      optimize = function() {
        invoke(
          nloptr::nloptr,
          eval_f = wrapper,
          lb = self$acq_function$domain$lower,
          ub = self$acq_function$domain$upper,
          opts = insert_named(pv, list(algorithm = "NLOPT_GN_DIRECT_L", maxeval = maxeval)),
          eval_grad_f = NULL,
          x0 = x0,
          fun = fun,
          constants = constants,
          direction = direction
        )
      }

      res = if (catch_errors) {
        tryCatch(optimize(), error = function(error_condition) {
          error_acq_optimizer("Acquisition function optimization failed.", parent = error_condition)
        })
      } else {
        optimize()
      }

      self$state = res

      as.data.table(as.list(set_names(
        c(res$solution, res$objective * direction),
        c(self$acq_function$domain$ids(), self$acq_function$codomain$ids())
      )))
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
      "(OptimizerDirect)"
    },

    #' @template field_label
    label = function(rhs) {
      assert_ro_binding(rhs)
      "DIRECT"
    },

    #' @template field_man
    man = function(rhs) {
      assert_ro_binding(rhs)
      "mlr3mbo::AcqOptimizerDirect"
    }
  )
)

mlr_acqoptimizers$add("direct", AcqOptimizerDirect)
