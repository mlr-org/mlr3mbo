#' @title Acquisition Function Optimizer
#'
#' @description
#' Optimizer for [AcqFunction]s which performs the acquisition function optimization.
#' Wraps an [bbotk::OptimizerBatch] and [bbotk::Terminator].
#'
#' @section Parameters:
#' \describe{
#' \item{`n_candidates`}{`integer(1)`\cr
#'   Number of candidates to propose.
#'   Note that this does not affect how the acquisition function itself is calculated (e.g., setting `n_candidates > 1` will not
#'   result in computing the q- or multi-Expected Improvement) but rather the top `n_candidates` are selected from the
#'   [bbotk::ArchiveBatch] of the acquisition function [bbotk::OptimInstanceBatch].
#'   Note that setting `n_candidates > 1` is usually not a sensible idea but it is still supported for experimental reasons.
#'   Note that in the case of the acquisition function [bbotk::OptimInstanceBatch] being multi-criteria, due to using an [AcqFunctionMulti],
#'   selection of the best candidates is performed via non-dominated-sorting.
#'   Default is `1`.
#' }
#' \item{`logging_level`}{`character(1)`\cr
#'   Logging level during the acquisition function optimization.
#'   Can be `"fatal"`, `"error"`, `"warn"`, `"info"`, `"debug"` or `"trace"`.
#'   Default is `"warn"`, i.e., only warnings are logged.
#' }
#' \item{`warmstart`}{`logical(1)`\cr
#'   Should the acquisition function optimization be warm-started by evaluating the best point(s) present in the [bbotk::Archive] of
#'   the actual [bbotk::OptimInstance] (which is contained in the archive of the [AcqFunction])?
#'   This is sensible when using a population based acquisition function optimizer, e.g., local search or mutation.
#'   Default is `FALSE`.
#'   Note that in the case of the [bbotk::OptimInstance] being multi-criteria, selection of the best point(s) is performed via non-dominated-sorting.
#'   Note that this warm-starting step cannot be influenced by callbacks.
#' }
#' \item{`warmstart_size`}{`integer(1) | "all"`\cr
#'   Number of best points selected from the [bbotk::Archive] of the actual [bbotk::OptimInstance] that are to be used for warm-starting.
#'   Can either be an integer or "all" to use all available points currently logged into the [bbotk::Archive] of the [bbotk::OptimInstance].
#'   Only relevant if `warmstart = TRUE`.
#'   Default is `1`.
#' }
#' \item{`skip_already_evaluated`}{`logical(1)`\cr
#'   It can happen that the candidate(s) resulting of the acquisition function optimization were already evaluated on the actual [bbotk::OptimInstance].
#'   Should such candidate proposals be ignored and only candidates that were yet not evaluated be considered?
#'   Default is `TRUE`.
#' }
#' \item{`refine_on_numeric_subspace`}{`logical(1)`\cr
#'   After the acquisition function optimization has been performed, should the optimization result be refined on the purely numeric subspace of the acquisition function domain?
#'   If `TRUE`, L-BFGS-B will be run on the subspace of the acquisition function domain containing only numeric parameters, keeping all other parameters constant at the value of the best solution found yet.
#'   As a starting value, the current best solution will be used.
#'   Only sensible for single-criteria acquisition functions with a `ydim` of `1`.
#'   Uses [stats::optim]'s L-BFGS-B implementation.
#'   Note that this is currently considered an experimental feature.
#'   Default is `FALSE`.
#'   Note that this refinement step cannot be influenced by callbacks.
#' }
#' \item{`catch_errors`}{`logical(1)`\cr
#'   Should errors during the acquisition function optimization be caught and propagated to the `loop_function` so that
#'   the failed acquisition function optimization can be handled appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Setting this to `FALSE` can be helpful for debugging.
#'   Default is `TRUE`.
#' }
#' }
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'   library(data.table)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = default_gp()
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("ei", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search", batch_size = 1000),
#'     terminator = trm("evals", n_evals = 1000),
#'     acq_function = acq_function)
#'
#'   acq_optimizer$optimize()
#' }
AcqOptimizer = R6Class("AcqOptimizer",
  public = list(

    #' @field optimizer ([bbotk::OptimizerBatch]).
    optimizer = NULL,

    #' @field terminator ([bbotk::Terminator]).
    terminator = NULL,

    #' @field acq_function ([AcqFunction]).
    acq_function = NULL,

    #' @field callbacks (`NULL` | list of [mlr3misc::Callback]).
    callbacks = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer ([bbotk::OptimizerBatch]).
    #' @param terminator ([bbotk::Terminator]).
    #' @param acq_function (`NULL` | [AcqFunction]).
    #' @param callbacks (`NULL` | list of [mlr3misc::Callback])
    initialize = function(optimizer, terminator, acq_function = NULL, callbacks = NULL) {
      self$optimizer = assert_r6(optimizer, "Optimizer")
      self$terminator = assert_r6(terminator, "Terminator")
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      self$callbacks = assert_callbacks(as_callbacks(callbacks))
      ps = ps(
        n_candidates = p_int(lower = 1, default = 1L),
        logging_level = p_fct(levels = c("fatal", "error", "warn", "info", "debug", "trace"), default = "warn"),
        warmstart = p_lgl(default = FALSE),
        warmstart_size = p_int(lower = 1L, special_vals = list("all")),
        skip_already_evaluated = p_lgl(default = TRUE),
        refine_on_numeric_subspace = p_lgl(default = FALSE),
        catch_errors = p_lgl(default = TRUE)
      )
      ps$values = list(n_candidates = 1, logging_level = "warn", warmstart = FALSE, skip_already_evaluated = TRUE, refine_on_numeric_subspace = FALSE, catch_errors = TRUE)
      ps$add_dep("warmstart_size", on = "warmstart", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Helper for print outputs.
    #'
    #' @return (`character(1)`).
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), paste0(": ", self$print_id))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values)))
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @return [data.table::data.table()] with 1 row per candidate.
    optimize = function() {
      is_multi_acq_function = self$acq_function$codomain$length > 1L

      lg = lgr::get_logger("bbotk")
      old_threshold = lg$threshold
      lg$set_threshold(self$param_set$values$logging_level)
      on.exit(lg$set_threshold(old_threshold))

      if (is_multi_acq_function) {
        instance = OptimInstanceBatchMultiCrit$new(objective = self$acq_function, search_space = self$acq_function$domain, terminator = self$terminator, check_values = FALSE, callbacks = self$callbacks)
      } else {
        instance = OptimInstanceBatchSingleCrit$new(objective = self$acq_function, search_space = self$acq_function$domain, terminator = self$terminator, check_values = FALSE, callbacks = self$callbacks)
      }

      # warmstart
      if (self$param_set$values$warmstart) {
        warmstart_size = if (isTRUE(self$param_set$values$warmstart_size == "all")) Inf else self$param_set$values$warmstart_size %??% 1L  # default is 1L
        n_select = min(nrow(self$acq_function$archive$data), warmstart_size)
        warmstart_xdt = if (is_multi_acq_function) {
          self$acq_function$archive$nds_selection(n_select = n_select)[, instance$search_space$ids(), with = FALSE]
        } else {
          self$acq_function$archive$best(n_select = n_select)[, instance$search_space$ids(), with = FALSE]
        }
        instance$eval_batch(warmstart_xdt)
      }

      # acquisition function optimization
      xdt = if (self$param_set$values$skip_already_evaluated) {
        if (self$param_set$values$catch_errors) {
          tryCatch(
            {
              self$optimizer$optimize(instance)
              get_best(instance, is_multi_acq_function = is_multi_acq_function, evaluated = self$acq_function$archive$data, n_select = self$param_set$values$n_candidates, not_already_evaluated = TRUE)
            }, error = function(error_condition) {
              lg$warn(error_condition$message)
              stop(set_class(list(message = error_condition$message, call = NULL),
                classes = c("acq_optimizer_error", "mbo_error", "error", "condition")))
            }
          )
        } else {
          self$optimizer$optimize(instance)
          get_best(instance, is_multi_acq_function = is_multi_acq_function, evaluated = self$acq_function$archive$data, n_select = self$param_set$values$n_candidates, not_already_evaluated = TRUE)
        }
      } else {
        if (self$param_set$values$catch_errors) {
          tryCatch(
            {
              self$optimizer$optimize(instance)
              get_best(instance, is_multi_acq_function = is_multi_acq_function, evaluated = self$acq_function$archive$data, n_select = self$param_set$values$n_candidates, not_already_evaluated = FALSE)
            }, error = function(error_condition) {
              lg$warn(error_condition$message)
              stop(set_class(list(message = error_condition$message, call = NULL),
                classes = c("acq_optimizer_error", "mbo_error", "error", "condition")))
            }
          )
        } else {
          self$optimizer$optimize(instance)
          get_best(instance, is_multi_acq_function = is_multi_acq_function, evaluated = self$acq_function$archive$data, n_select = self$param_set$values$n_candidates, not_already_evaluated = FALSE)
        }
      }

      # refine
      if (self$param_set$values$refine_on_numeric_subspace && !is_multi_acq_function && any(self$acq_function$domain$class == "ParamDbl")) {
        lg$info("Refining the acquisition function optimization result on the purely numeric subspace of the acquisition function domain via L-BFGS-B")
        instance$terminator = trm("none")  # allow for additionally running L-BFGS-B converging on its own
        current_best = as.list(xdt[1L, instance$search_space$ids(), with = FALSE])  # not x_domain because acquisition functions are optimized on the search space and trafos have been nulled
        ids = instance$search_space$ids()
        assert_true(all(ids == names(current_best)))  # order matters below
        params_to_refine = intersect(instance$search_space$ids(class = "ParamDbl"), ids[!map_lgl(current_best, is.na)])
        params_constant = setdiff(ids, params_to_refine)
        constants = current_best[params_constant]
        lower = instance$search_space$lower[params_to_refine]
        upper = instance$search_space$upper[params_to_refine]
        # L-BFGS-B evaluations are logged as usual into the archive
        lbfgsb = stats::optim(par = unlist(current_best[params_to_refine]),
                         fn = wrap_acq_function_lbfgsb,
                         acquisition_function_instance = instance,
                         constants = constants,
                         method = "L-BFGS-B",
                         lower = lower,
                         upper = upper)
        xdt = get_best(instance, is_multi_acq_function = is_multi_acq_function, evaluated = self$acq_function$archive$data, n_select = self$param_set$values$n_candidates, not_already_evaluated = FALSE)
      }
      #if (is_multi_acq_function) {
      #  set(xdt, j = instance$objective$id, value = apply(xdt[, instance$objective$acq_function_ids, with = FALSE], MARGIN = 1L, FUN = c, simplify = FALSE))
      #  for (acq_function_id in instance$objective$acq_function_ids) {
      #    set(xdt, j = acq_function_id, value = NULL)
      #  }
      #  setcolorder(xdt, c(instance$archive$cols_x, "x_domain", instance$objective$id))
      #}
      xdt[, -c("timestamp", "batch_nr")]  # drop timestamp and batch_nr information from the candidates
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

