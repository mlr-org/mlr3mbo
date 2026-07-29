#' @title CMA-ES Acquisition Function Optimizer
#'
#' @include AcqOptimizer.R mlr_acqoptimizers.R
#'
#' @description
#' CMA-ES acquisition function optimizer.
#' Calls `cmaes()` from \CRANpkg{libcmaesr}.
#' The default algorithm is `"abipop"` with unlimited restarts and a budget of `100 * D^2` function evaluations,
#' where `D` is the dimension of the search space.
#' The optimization starts from the best point in the archive.
#' For the meaning of the control parameters, see `libcmaesr::cmaes_control()`.
#'
#' Only fully numeric search spaces (all parameters of type `p_dbl`) are supported.
#'
#' @section Parameters:
#' \describe{
#' \item{`algo`}{`character(1)`\cr
#'   CMA-ES variant to use, see `libcmaesr::cmaes_algos`.
#'   Default is `"abipop"`.}
#' \item{`lambda`}{`integer(1)`\cr
#'   Number of generated descendants per iteration.
#'   Deactivate with `NA` (Default).}
#' \item{`sigma`}{`numeric(1)`\cr
#'   Initial sigma for the covariance.
#'   Deactivate with `NA` (Default).}
#' \item{`max_restarts`}{`integer(1)`\cr
#'   Maximum number of restarts for the IPOP and BIPOP variants.
#'   Default is `1e5`, i.e., restarts are only limited by the evaluation budget.
#'   Deactivate with `NA`.}
#' \item{`tpa`}{`integer(1)`\cr
#'   Activates or deactivates the two-point adaptation step-size mechanism.
#'   `0` for no, `1` for auto, `2` for yes.
#'   Deactivate with `NA` (Default).}
#' \item{`tpa_dsigma`}{`numeric(1)`\cr
#'   Value of the two-point adaptation dsigma.
#'   Deactivate with `NA` (Default).}
#' \item{`seed`}{`integer(1)`\cr
#'   Seed of the random number generator of libcmaes.
#'   If `NA` (Default), the seed is drawn from R and the optimization is therefore reproducible via `set.seed()`.}
#' \item{`quiet`}{`logical(1)`\cr
#'   Should the output of libcmaes be suppressed?
#'   Default is `TRUE`.}
#' \item{`skip_already_evaluated`}{`logical(1)`\cr
#'   Should the proposed candidate be rejected if it was already evaluated on the actual [bbotk::OptimInstance]?
#'   If `TRUE` and the candidate was already evaluated, an error is raised so that the `loop_function` can
#'   propose a randomly sampled point instead.
#'   Default is `TRUE`.}
#' }
#'
#' @section Termination Parameters:
#' The following termination parameters can be used.
#'
#' \describe{
#' \item{`max_fevals`}{`integer(1)`\cr
#'   Maximum number of function evaluations.
#'   Default is `100 * D^2`, where `D` is the dimension of the search space.
#'   Deactivate with `NA`.}
#' \item{`max_iter`}{`integer(1)`\cr
#'   Maximum number of iterations.
#'   Deactivate with `NA` (Default).}
#' \item{`ftarget`}{`numeric(1)`\cr
#'   Target function value.
#'   Deactivate with `NA` (Default).}
#' \item{`f_tolerance`}{`numeric(1)`\cr
#'   Function tolerance.
#'   Deactivate with `NA` (Default).}
#' \item{`x_tolerance`}{`numeric(1)`\cr
#'   Parameter tolerance.
#'   Deactivate with `NA` (Default).}
#' }
#'
#' @export
#' @examples
#' if (requireNamespace("libcmaesr")) {
#'   acqo("cmaes")
#' }
AcqOptimizerCmaes = R6Class(
  "AcqOptimizerCmaes",
  inherit = AcqOptimizer,
  public = list(
    #' @field state (`list()`)\cr
    #' Result of the last `libcmaesr::cmaes()` run.
    state = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      param_set = ps(
        algo = p_fct(
          levels = c(
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
            "vdbipopcma"
          ),
          init = "abipop"
        ),
        lambda = p_int(lower = 2L, default = NA_integer_, special_vals = list(NA_integer_)),
        sigma = p_dbl(lower = 0, default = NA_real_, special_vals = list(NA_real_)),
        max_restarts = p_int(lower = 0L, special_vals = list(NA_integer_), init = 1e5L),
        tpa = p_int(lower = 0L, upper = 2L, default = NA_integer_, special_vals = list(NA_integer_)),
        tpa_dsigma = p_dbl(lower = 0, default = NA_real_, special_vals = list(NA_real_)),
        seed = p_int(default = NA_integer_, special_vals = list(NA_integer_)),
        quiet = p_lgl(default = TRUE),
        # internal termination criteria
        max_fevals = p_int(lower = 1L, special_vals = list(NA_integer_)),
        max_iter = p_int(lower = 1L, default = NA_integer_, special_vals = list(NA_integer_)),
        ftarget = p_dbl(default = NA_real_, special_vals = list(NA_real_)),
        f_tolerance = p_dbl(lower = 0, default = NA_real_, special_vals = list(NA_real_)),
        x_tolerance = p_dbl(lower = 0, default = NA_real_, special_vals = list(NA_real_)),
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
        stopf("`AcqOptimizerCmaes` only supports fully numeric (`p_dbl`) search spaces.")
      }
      self$state = NULL
      pv = self$param_set$values
      catch_errors = pv$catch_errors
      skip_already_evaluated = pv$skip_already_evaluated

      if (is.null(pv$max_fevals)) {
        pv$max_fevals = 100 * self$acq_function$domain$length^2
      }

      fun = get_private(self$acq_function)$.fun
      constants = self$acq_function$constants$values
      direction = self$acq_function$codomain$direction
      ids = self$acq_function$domain$ids()

      # libcmaesr minimizes by default and handles maximization internally, i.e., the reported objective value is
      # always on the original scale of the acquisition function
      control = invoke(
        libcmaesr::cmaes_control,
        maximize = direction == -1L,
        .args = pv[names(pv) %in% names(formals(libcmaesr::cmaes_control))]
      )

      wrapper = function(xmat) {
        xdt = set_names(as.data.table(xmat), ids)
        mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1L]]
      }

      lower = self$acq_function$domain$lower
      upper = self$acq_function$domain$upper
      # libcmaesr requires the starting point to lie within the bounds; the incumbent can be marginally out of bounds
      # due to floating point noise, e.g., after a search space transformation
      x0 = as.numeric(self$acq_function$archive$best()[, ids, with = FALSE])
      x0 = pmin(pmax(x0, lower), upper)

      optimize = function() {
        libcmaesr::cmaes(
          objective = wrapper,
          x0 = x0,
          lower = lower,
          upper = upper,
          batch = TRUE,
          control = control
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

      xdt = as.data.table(as.list(set_names(
        c(res$x, res$y),
        c(ids, self$acq_function$codomain$ids())
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
      "(OptimizerCmaes)"
    },

    #' @template field_label
    label = function(rhs) {
      assert_ro_binding(rhs)
      "CMA-ES"
    },

    #' @template field_man
    man = function(rhs) {
      assert_ro_binding(rhs)
      "mlr3mbo::AcqOptimizerCmaes"
    }
  )
)

mlr_acqoptimizers$add("cmaes", AcqOptimizerCmaes)
