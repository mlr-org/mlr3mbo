#' @title CMA-ES Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R
#'
#' @description
#' CMA-ES acquisition function optimizer.
#' Calls `cmaes()` from \CRANpkg{libcmaesr}.
#' The default algorithm is `"abipop"` with unlimited restarts and a budget of `100 * D^2` function evaluations, where `D` is the dimension of the search space.
#' For the meaning of the control parameters, see `libcmaesr::cmaes_control()`.
#'
#' @section Termination Parameters:
#' The following termination parameters can be used.
#'
#' \describe{
#' \item{`max_fevals`}{`integer(1)`\cr
#'   Maximum number of function evaluations.
#'   Deactivate with `NA`.
#'   Default is `100 * D^2`, where `D` is the dimension of the search space.}
#' \item{`max_iter`}{`integer(1)`\cr
#'   Maximum number of iterations.
#'   Deactivate with `NA`.}
#' \item{`ftarget`}{`numeric(1)`\cr
#'   Target function value.
#'   Deactivate with `NA`.}
#' \item{`f_tolerance`}{`numeric(1)`\cr
#'   Function tolerance.
#'   Deactivate with `NA`.}
#' \item{`x_tolerance`}{`numeric(1)`\cr
#'   Parameter tolerance.
#'   Deactivate with `NA`.}
#' }
#'
#' @export
AcqOptimizerCmaes = R6Class("AcqOptimizerCmaes",
  inherit = AcqOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      param_set = ps(
        algo          = p_fct(init = "abipop", levels = c(
          "cmaes",
          "ipop",
          "bipop",
          "acmaes",
          "aipop",
          "abipop",
          "sepcmaes",
          "sepipop",
          "sepbipop",
          "sepacmaes",
          "sepaipop",
          "sepabipop",
          "vdcma",
          "vdipopcma",
          "vdbipopcma")),
        lambda        = p_int(lower = 1L, default = NA_integer_, special_vals = list(NA_integer_)),
        sigma         = p_dbl(default = NA_real_, special_vals = list(NA_real_)),
        max_restarts  = p_int(lower = 1L, special_vals = list(NA), init = 1e5L),
        tpa           = p_int(default = NA_integer_, special_vals = list(NA_integer_)),
        tpa_dsigma    = p_dbl(default = NA_real_, special_vals = list(NA_real_)),
        seed          = p_int(default = NA_integer_, special_vals = list(NA_integer_)),
        quiet         = p_lgl(default = FALSE),
        # internal termination criteria
        max_fevals    = p_int(lower = 1L, special_vals = list(NA_integer_)),
        max_iter      = p_int(lower = 1L, default = NA_integer_, special_vals = list(NA_integer_)),
        ftarget       = p_dbl(default = NA_real_, special_vals = list(NA_real_)),
        f_tolerance   = p_dbl(default = NA_real_, special_vals = list(NA_real_)),
        x_tolerance   = p_dbl(default = NA_real_, special_vals = list(NA_real_)),
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

      if (is.null(pv$max_fevals)) {
        pv$max_fevals = 100 * self$acq_function$domain$length^2
      }

      fun = get_private(self$acq_function)$.fun
      constants = self$acq_function$constants$values
      direction = self$acq_function$codomain$direction

      control = invoke(libcmaesr::cmaes_control, maximize = direction == -1L, .args = pv[names(pv) %in% formalArgs(libcmaesr::cmaes_control)])

      wrapper = function(xmat) {
        xdt = set_names(as.data.table(xmat), self$acq_function$domain$ids())
        mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
      }

      lower = self$acq_function$domain$lower
      upper = self$acq_function$domain$upper
      x0 = as.numeric(self$acq_function$archive$best()[, self$acq_function$domain$ids(), with = FALSE])

      # add saveguard_epsilon to x0
      saveguard_epsilon = 1e-5
      x0[x0 < lower] = x0[x0 < lower] + saveguard_epsilon
      x0[x0 > upper] = x0[x0 > upper] - saveguard_epsilon

      optimize = function() {
        libcmaesr::cmaes(
          objective = wrapper,
          x0 = x0,
          lower = lower,
          upper = upper,
          batch = TRUE,
          control = control)
      }

      if (pv$catch_errors) {
        tryCatch({
          res = optimize()
        }, error = function(error_condition) {
          lg$warn(error_condition$message)
          stop(set_class(list(message = error_condition$message, call = NULL), classes = c("acq_optimizer_error", "mbo_error", "error", "condition")))
        })
      } else {
        res = optimize()
      }
      as.data.table(as.list(set_names(c(res$x, res$y * direction), c(self$acq_function$domain$ids(), self$acq_function$codomain$ids()))))
    }
  ),

  active = list(
    #' @template field_print_id
    print_id = function(rhs) {
      assert_ro_binding(rhs)
      "(OptimizerBatchCmaes)"
    }
  )
)

