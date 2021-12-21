#' @title Global Followed by Local Optimization
#'
#' @include Optimizer.R
#' @name mlr_optimizers_global_local
#'
#' @description
#' `OptimizerGlobalLocal` class that starts with `min(2000, 500d)` random evaluations (with `d` being the
#' dimensionality of the search space) and then uses the thereby best obtained point to further optimize via L-BFGS-B.
#'
#' @templateVar id global_local
#' @template section_dictionary_optimizers
#'
#' @template section_progress_bars
#'
#' @export
#' @template example
OptimizerGlobalLocal = R6Class("OptimizerGlobalLocal",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps()  # FIXME: expose optim params etc.

      super$initialize(
        param_set = param_set,
        param_classes = "ParamDbl",
        properties = "single-crit"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      # start with random
      # use best point to run L-BFGS-B
      # https://arxiv.org/pdf/2103.16649.pdf

      if (!inherits(inst$terminator, "TerminatorNone")) {
        stop("Terminator must be set to TerminatorNone.")
      }

      repeats = min(10L, length(inst$archive$cols_x))

      n_random = min(2000L, 500L * length(inst$archive$cols_x))

      optim_fun = function(x) {
        xdt = setNames(as.data.table(t(x)), nm = inst$archive$cols_x)
        y = as.numeric(inst$eval_batch(xdt))
        y
      }

      for (i in seq_len(repeats)) {
        random_design = generate_design_random(inst$search_space, n = n_random)
        inst$eval_batch(random_design$data)
        starting_point = inst$archive$best(batch = inst$archive$n_batch)
        optim(as.numeric(starting_point[, inst$archive$cols_x, with = FALSE]), fn = optim_fun, method = "L-BFGS-B", lower = inst$search_space$lower, upper = inst$search_space$upper, control = list(fnscale = inst$objective_multiplicator))
      }
    }
  )
)

mlr_optimizers$add("global_local", OptimizerGlobalLocal)
