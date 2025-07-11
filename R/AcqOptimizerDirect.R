#' @export
AcqOptimizerDirect = R6Class("AcqOptimizerDirect",
  inherit = AcqOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      ps = ps(
        stopval = p_dbl(default = -Inf, lower = -Inf, upper = Inf),
        xtol_rel = p_dbl(default = 1e-06, lower = 0, upper = Inf, special_vals = list(-1)),
        xtol_abs = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1)),
        maxeval = p_int(lower = 1, default = 1000L, special_vals = list(-1)),
        ftol_rel = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1)),
        ftol_abs = p_dbl(default = 0, lower = 0, upper = Inf, special_vals = list(-1)),
        minf_max = p_dbl(default = -Inf)
        # n_candidates = p_int(lower = 1, default = 1L),
        # logging_level = p_fct(levels = c("fatal", "error", "warn", "info", "debug", "trace"), default = "warn"),
        # warmstart = p_lgl(default = FALSE),
        # warmstart_size = p_int(lower = 1L, special_vals = list("all")),
        # skip_already_evaluated = p_lgl(default = TRUE),
        # catch_errors = p_lgl(default = TRUE)
      )
      # ps$values = list(n_candidates = 1, logging_level = "warn", warmstart = FALSE, skip_already_evaluated = TRUE, catch_errors = TRUE)
      # ps$add_dep("warmstart_size", on = "warmstart", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      pv = self$param_set$values
      x0 = as.numeric(self$acq_function$archive$best()[, self$acq_function$domain$ids(), with = FALSE])

      wrapper = function(x, fun, constants, direction) {
        xdt = as.data.table(as.list(set_names(x, self$acq_function$domain$ids())))
        res = mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
        res * direction
      }

      fun = get_private(self$acq_function)$.fun
      constants = self$acq_function$constants$values
      direction = self$acq_function$codomain$direction

      res = invoke(nloptr::nloptr,
        eval_f = wrapper,
        lb = self$acq_function$domain$lower,
        ub = self$acq_function$domain$upper,
        opts = c(pv, list(algorithm = "NLOPT_GN_DIRECT_L")),
        eval_grad_f = NULL,
        x0 = x0,
        fun = fun,
        constants = constants,
        direction = direction)

      as.data.table(as.list(set_names(c(res$solution, res$objective * direction), c(self$acq_function$domain$ids(), self$acq_function$codomain$ids()))))
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

