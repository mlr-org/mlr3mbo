#' @title Sequential Multi-Objective Bayesian Optimization
#'
#' @include mlr_loop_functions.R
#' @name mlr_loop_functions_emo
#'
#' @description
#' Loop function for sequential multi-objective Bayesian Optimization.
#' Normally used inside an [OptimizerMbo].
#' The conceptual counterpart to [mlr_loop_functions_ego].
#'
#' In each iteration after the initial design, the surrogate and acquisition function are updated and the next candidate
#' is chosen based on optimizing the acquisition function.
#'
#' @param instance ([bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstanceMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` and the [bbotk::Archive] contains no evaluations, \code{4 * d} is used with \code{d} being the
#'   dimensionality of the search space.
#'   Points are generated via a Sobol sequence.
#' @param surrogate ([SurrogateLearnerCollection])\cr
#'   [SurrogateLearnerCollection] to be used as a surrogate.
#' @param acq_function ([AcqFunction])\cr
#'   [AcqFunction] to be used as acquisition function.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#' @param random_interleave_iter (`integer(1)`)\cr
#'   Every `random_interleave_iter` iteration (starting after the initial design), a point is
#'   sampled uniformly at random and evaluated (instead of a model based proposal).
#'   For example, if `random_interleave_iter = 2`, random interleaving is performed in the second,
#'   fourth, sixth, ... iteration.
#'   Default is `0`, i.e., no random interleaving is performed at all.
#'
#' @note
#' * The `acq_function$surrogate`, even if already populated, will always be overwritten by the `surrogate`.
#' * The `acq_optimizer$acq_function`, even if already populated, will always be overwritten by `acq_function`.
#' * The `surrogate$archive`, even if already populated, will always be overwritten by the [bbotk::Archive] of the [bbotk::OptimInstanceMultiCrit].
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @family Loop Function
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceMultiCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   surrogate = default_surrogate(instance)
#'
#'   acq_function = acqf("ehvi")
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search", batch_size = 100),
#'     terminator = trm("evals", n_evals = 100))
#'
#'   optimizer = opt("mbo",
#'     loop_function = bayesopt_emo,
#'     surrogate = surrogate,
#'     acq_function = acq_function,
#'     acq_optimizer = acq_optimizer)
#'
#'   optimizer$optimize(instance)
#' }
#' }
bayesopt_emo = function(
    instance,
    surrogate,
    acq_function,
    acq_optimizer,
    init_design_size = NULL,
    random_interleave_iter = 0L
  ) {

  # assertions
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_r6(surrogate, classes = "SurrogateLearnerCollection")
  assert_r6(acq_function, classes = "AcqFunction")
  assert_r6(acq_optimizer, classes = "AcqOptimizer")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_int(random_interleave_iter, lower = 0L)

  # initial design
  search_space = instance$search_space
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) {
    init_design_size = 4L * search_space$length
  }
  if (!is.null(init_design_size) && init_design_size > 0L && instance$archive$n_evals == 0L) {
    design = generate_design_sobol(search_space, n = init_design_size)$data
    instance$eval_batch(design)
  }

  # completing initialization
  surrogate$archive = instance$archive
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  # actual loop
  repeat {
    xdt = tryCatch({
      # random interleaving is handled here
      if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
        stop(set_class(list(message = "Random interleaving", call = NULL), classes = c("random_interleave", "mbo_error", "error", "condition")))
      }
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      lg$info(paste0(class(mbo_error_condition), collapse = " / "))
      lg$info("Proposing a randomly sampled point")
      generate_design_random(search_space, n = 1L)$data
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

class(bayesopt_emo) = "loop_function"
attr(bayesopt_emo, "id") = "bayesopt_emo"
attr(bayesopt_emo, "label") = "Multi-Objective EGO"
attr(bayesopt_emo, "instance") = "multi-crit"
attr(bayesopt_emo, "man") = "mlr3mbo::mlr_loop_functions_emo"

mlr_loop_functions$add("bayesopt_emo", bayesopt_emo)

