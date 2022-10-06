#' @title Sequential Multicriteria Bayesian Optimization via SmsEGO
#'
#' @include mlr_loop_functions.R
#' @name mlr_loop_functions_smsego
#'
#' @description
#' MBO loop function for sequential multicriteria Bayesian optimization via SmsEGO.
#' Normally used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstanceMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param surrogate (`NULL` | [SurrogateLearnerCollection])\cr
#'   [SurrogateLearnerCollection] to be used as a surrogate.
#'   If `NULL` \code{default_surrogate(instance)} is used.
#' @param acq_function (`NULL` | [AcqFunctionSmsEgo]).\cr
#'   [AcqFunctionSmsEgo] to be used as acquisition function.
#'   If `NULL` an [AcqFunctionSmsEgo] is used.
#' @param acq_optimizer (`NULL` | [AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   If `NULL` \code{default_acqopt(acqfun)} is used.
#' @param random_interleave_iter (`integer(1)`)\cr
#'   Every "random_interleave_iter" iteration (starting after the initial design), a point is
#'   sampled uniformly at random and evaluated (instead of a model based proposal).
#'   For example, if `random_interleave_iter = 2`, random interleaving is performed in the second,
#'   fourth, sixth, ... iteration.
#'   Default is `0`, i.e., no random interleaving is performed at all.
#'
#' @note
#' * If `surrogate` is `NULL` but `acq_function` is given and contains a `$surrogate`, this
#'   [SurrogateLearnerCollection] is used.
#' * You can pass a `surrogate` that was not given the [bbotk::Archive] of the
#'   `instance` during initialization.
#'   In this case, the [bbotk::Archive] of the given `instance` is set during execution.
#' * Similarly, you can pass an `acq_function` that was not given the `surrogate` during initialization
#'   and an `acq_optimizer` that was not given the `acq_function`, i.e., delayed initialization is
#'   handled automatically.
#' * Due to the iterative computation of the epsilon within the [AcqFunctionSmsEgo], requires the [bbotk::Terminator] of
#'   the [bbotk::OptimInstanceMultiCrit] to be a [bbotk::TerminatorEvals].
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("beume_2007")`
#' * `r format_bib("ponweiser_2008")`
#' @family Loop Function
#' @export
#' @examples
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
#'   terminator = trm("evals", n_evals = 5)
#'
#'   instance = OptimInstanceMultiCrit$new(
#'     objective = objective,
#'     terminator = terminator
#'   )
#'
#'   bayesopt_smsego(instance)
#' }
bayesopt_smsego = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL,
    random_interleave_iter = 0L
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_r6(instance$terminator, "TerminatorEvals")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "SurrogateLearnerCollection", null.ok = TRUE)
  assert_r6(acq_function, classes = "AcqFunctionSmsEgo", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
  assert_int(random_interleave_iter, lower = 0L)

  surrogate = surrogate %??% acq_function$surrogate

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  k = length(archive$cols_y)  # codomain can hold non targets since #08116aa02204980f87c8c08841176ae8f664980a
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4L * d
  if (is.null(surrogate)) surrogate = default_surrogate(instance)
  if (is.null(acq_function)) acq_function = AcqFunctionSmsEgo$new()
  if (is.null(acq_optimizer)) acq_optimizer = default_acqopt(acq_function)
  surrogate$archive = archive
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  } else {
    init_design_size = instance$archive$n_evals
  }

  # loop
  repeat {
    xdt = tryCatch({
      # random interleaving is handled here
      if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
        stop(set_class(list(message = "Random interleaving", call = NULL), classes = c("random_interleave", "mbo_error", "error", "condition")))
      }
      acq_function$progress = instance$terminator$param_set$values$n_evals - archive$n_evals
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      lg$info(paste0(class(mbo_error_condition), collapse = " / "))
      lg$info("Proposing a randomly sampled point")
      SamplerUnif$new(domain)$sample(1L)$data
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

class(bayesopt_smsego) = "loop_function"
attr(bayesopt_smsego, "id") = "bayesopt_smsego"
attr(bayesopt_smsego, "label") = "SmsEGO"
attr(bayesopt_smsego, "instance") = "multi-crit"
attr(bayesopt_smsego, "man") = "mlr3mbo::mlr_loop_functions_smsego"

mlr_loop_functions$add("bayesopt_smsego", bayesopt_smsego)
