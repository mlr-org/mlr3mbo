#' @title Acquisition Function Optimizer
#'
#' @description
#' Optimizer for [AcqFunction]s.
#' Wraps an [bbotk::Optimizer] and [bbotk::Terminator].
#'
#' @export
AcqOptimizer = R6Class("AcqOptimizer",
  public = list(

    #' @field optimizer ([bbotk::Optimizer]).
    optimizer = NULL,

    #' @field terminator ([bbotk::Terminator]).
    terminator = NULL,

    #' @field acq_function ([AcqFunction]).
    acq_function = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizer ([bbotk::Optimizer]).
    #' @param terminator ([bbotk::Terminator]).
    #' @param acq_function (`NULL` | [AcqFunction]).
    initialize = function(optimizer, terminator, acq_function = NULL) {
      self$optimizer = assert_r6(optimizer, "Optimizer")
      self$terminator = assert_r6(terminator, "Terminator")
      self$acq_function = assert_r6(acq_function, "AcqFunction", null.ok = TRUE)
      ps = ps(
        logging_level = p_fct(levels = c("fatal", "error", "warn", "info", "debug", "trace"), default = "warn"),
        warmstart = p_lgl(default = FALSE),
        warmstart_size = p_int(lower = 1L),
        fix_distance = p_lgl(default = FALSE),
        dist_threshold = p_dbl(lower = 0, upper = 1)
      )

      # FIXME: not having a dist_threshold may be problematic if only fix_distance is set to TRUE
      # FIXME: assert this
      ps$values = list(logging_level = "warn", warmstart = FALSE, fix_distance = FALSE)
      ps$add_dep("warmstart_size", on = "warmstart", cond = CondEqual$new(TRUE))
      ps$add_dep("dist_threshold", on = "fix_distance", cond = CondEqual$new(TRUE))
      private$.param_set = ps
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' If the `fix_distance` parameter is set to `TRUE`, proposed points are
    #' replaced by randomly sampled ones if their Gower distance with respect
    #' to other proposed points or previously evaluated points falls below the
    #' `dist_threshold` parameter. Note that these checks are only performed a
    #' single time (and not after a potential replacement of points), i.e., the
    #' returned [data.table::data.table()] of optima must not necessarily pass
    #' the checks but most likely will.
    #'
    #' @return [data.table::data.table()] with 1 row per optimum and x as columns.
    optimize = function() {
      logger = lgr::get_logger("bbotk")
      old_threshold = logger$threshold
      logger$set_threshold(self$param_set$values$logging_level)

      instance = if (self$acq_function$codomain$length == 1L) {
        OptimInstanceSingleCrit$new(objective = self$acq_function, search_space = self$acq_function$domain, terminator = self$terminator, check_values = FALSE, keep_evals = "all")
      } else {
        if (!"multi-crit" %in% self$optimizer$properties) {
          stopf("Optimizer %s is not multi-crit compatible but %s is multi-crit.", self$self$optimizer$format(), self$acq_function$id)
        }
        OptimInstanceMultiCrit$new(objective = self$acq_function, search_space = self$acq_function$domain, terminator = self$terminator, check_values = FALSE, keep_evals = "all")
      }

      if (self$param_set$values$warmstart) {
        # NOTE: is this save if e.g. mu < nrow(best) in miesmuschel?
        n_select = min(nrow(self$acq_function$archive$data), self$param_set$values$warmstart_size)
        instance$eval_batch(self$acq_function$archive$best(n_select = n_select)[, instance$search_space$ids(), with = FALSE])
      }

      xdt = tryCatch(self$optimizer$optimize(instance),
        error = function(error_condition) {
          lg$info(error_condition$message)
          # FIXME: this could potentially also fail if the surrogate cannot predict?
          stop(set_class(list(message = error_condition$message, call = NULL),
            classes = c("mbo_error", "acq_optimizer_error", "error", "condition")))
        }
      )

      if (self$param_set$values$fix_distance) {
        xdt = fix_xdt_distance(xdt, previous_xdt = archive_x(self$acq_function$archive), search_space = self$acq_function$domain, dist_threshold = self$param_set$values$dist_threshold)
      }

      logger$set_threshold(old_threshold)

      xdt
    }
  ),

  active = list(

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of hyperparameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
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
