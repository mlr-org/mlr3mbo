#' @title CMA-ES Acquisition Function Optimizer
#'
#' @export
AcqOptimizerCmaes = R6Class("AcqOptimizerCmaes",
  inherit = AcqOptimizer,
  public = list(

    #' @field state (`list()`)\cr
    #' [libcmaesr::cmaes()] results.
    state = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(acq_function = NULL) {
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      param_set = ps(
        max_fevals = p_int(lower = 1, init = 1000L)
        # maxeval          = p_int(lower = 1, init = 1000L, special_vals = list(-1)),
        # fnscale          = p_dbl(default = 1),
        # #maxit            = p_int(lower  = 1L),
        # stopfitness      = p_dbl(default = -Inf),
        # keep.best        = p_lgl(default = TRUE),
        # sigma            = p_uty(default = 0.5),
        # mu               = p_int(lower = 1L),
        # lambda           = p_int(lower = 1L),
        # weights          = p_uty(),
        # damps            = p_dbl(),
        # cs               = p_dbl(),
        # ccum             = p_dbl(),
        # ccov.1           = p_dbl(lower = 0),
        # ccov.mu          = p_dbl(lower = 0),
        # diag.sigma       = p_lgl(default = FALSE),
        # diag.eigen       = p_lgl(default = FALSE),
        # diag.pop         = p_lgl(default = FALSE),
        # diag.value       = p_lgl(default = FALSE),
        # stop.tolx        = p_dbl(), # undocumented stop criterion
        # restart_strategy = p_fct(levels = c("none", "ipop"), init = "none"),
        # n_restarts       = p_int(lower = 0L, init = 0L),
        # population_multiplier = p_int(lower = 1, init = 2L)
        # start_values  = p_fct(default = "random", levels = c("random", "center", "custom")),
        # start         = p_uty(default = NULL, depends = start_values == "custom"),
        # n_candidates = p_int(lower = 1, default = 1L),
        # logging_level = p_fct(levels = c("fatal", "error", "warn", "info", "debug", "trace"), default = "warn"),
        # warmstart = p_lgl(default = FALSE),
        # warmstart_size = p_int(lower = 1L, special_vals = list("all")),
        # skip_already_evaluated = p_lgl(default = TRUE),
        # catch_errors = p_lgl(default = TRUE)
      )
      # ps$values = list(n_candidates = 1, logging_level = "warn", warmstart = FALSE, skip_already_evaluated = TRUE, catch_errors = TRUE)
      # ps$add_dep("warmstart_size", on = "warmstart", cond = CondEqual$new(TRUE))
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

      control = libcmaesr::cmaes_control(
        maximize = direction == -1L,
        algo = "abipop",
        max_fevals = pv$max_fevals
      )

      wrapper = function(xmat) {
        xdt = set_names(as.data.table(xmat), self$acq_function$domain$ids())
        mlr3misc::invoke(fun, xdt = xdt, .args = constants)[[1]]
      }

      lower = self$acq_function$domain$lower
      upper = self$acq_function$domain$upper
      x0 = as.numeric(self$acq_function$archive$best()[, self$acq_function$domain$ids(), with = FALSE])

      res = libcmaesr::cmaes(
        objective = wrapper,
        x0 = x0,
        lower = lower,
        upper = upper,
        batch = TRUE,
        control = control)

      self$state = res

      as.data.table(as.list(set_names(c(res$x, res$y * direction), c(self$acq_function$domain$ids(), self$acq_function$codomain$ids()))))
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

