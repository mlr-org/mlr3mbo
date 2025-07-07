#' @export
AcqOptimizerDirect = R6Class("AcqOptimizerDirect",
  inherit = AcqOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer ([bbotk::OptimizerBatch]).
    #' @param terminator ([bbotk::Terminator]).
    #' @param acq_function (`NULL` | [AcqFunction]).
    #' @param callbacks (`NULL` | list of [mlr3misc::Callback])
    initialize = function(terminator, acq_function = NULL) {
      self$terminator = assert_r6(terminator, "Terminator")
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      ps = ps(
        n_candidates = p_int(lower = 1, default = 1L),
        logging_level = p_fct(levels = c("fatal", "error", "warn", "info", "debug", "trace"), default = "warn"),
        warmstart = p_lgl(default = FALSE),
        warmstart_size = p_int(lower = 1L, special_vals = list("all")),
        skip_already_evaluated = p_lgl(default = TRUE),
        catch_errors = p_lgl(default = TRUE)
      )
      ps$values = list(n_candidates = 1, logging_level = "warn", warmstart = FALSE, skip_already_evaluated = TRUE, catch_errors = TRUE)
      ps$add_dep("warmstart_size", on = "warmstart", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      lg = lgr::get_logger("mlr3/bbotk")
      old_threshold = lg$threshold
      lg$set_threshold(self$param_set$values$logging_level)
      on.exit(lg$set_threshold(old_threshold))

      if (inherits(self$terminator, "TerminatorEvals")) {
        maxeval = self$terminator$param_set$values$n_evals
      } else {
        stopf("Only TerminatorEvals is supported for AcqOptimizerDirect.")
      }

      wrapper = function(x) {
        constants = self$acq_function$constants$values
        as.numeric(mlr3misc::invoke(get_private(self$acq_function)$.fun, xdt = as.data.table(as.list(set_names(x, self$acq_function$domain$ids()))), .args = constants))
      }

      x0 = as.numeric(self$acq_function$archive$best()[, self$acq_function$domain$ids(), with = FALSE])

      res = invoke(nloptr::nloptr,
        eval_f = wrapper,
        lb = self$acq_function$domain$lower,
        ub = self$acq_function$domain$upper,
        opts = list(algorithm = "NLOPT_GN_DIRECT_L", maxeval = maxeval),
        eval_grad_f = NULL,
        x0 = x0)

      as.data.table(as.list(set_names(res$solution, self$acq_function$domain$ids())))
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

