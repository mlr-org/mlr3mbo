#' @title Sequential Single-Objective Bayesian Optimization With Log Transformation
#'
#' @include mlr_loop_functions.R
#' @name mlr_loop_functions_ego_log
#'
#' @description
#' Loop function for sequential single-objective Bayesian Optimization.
#' Normally used inside an [OptimizerMbo].
#'
#' In each iteration after the initial design, the surrogate and acquisition function are updated and the next candidate
#' is chosen based on optimizing the acquisition function.
#'
#' The target variable is multiplicatively corrected to be minimized, min-max scaled and then log transformed:
#' * \eqn{y = -1 y} if y is to be maximized
#' * \eqn{y_{\mathrm{transformed}} = \log(\frac{(y - \widehat{\min_{y})}}{(\max_{y} - \widehat{\min_y})}}
#' * where \eqn{\widehat{\min_{y}} = \min_{y} - \epsilon (\max_{y} - \min_{y})}
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit])\cr
#'   The [bbotk::OptimInstanceSingleCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` and the [bbotk::Archive] contains no evaluations, \code{4 * d} is used with \code{d} being the
#'   dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param surrogate ([Surrogate])\cr
#'   [Surrogate] to be used as a surrogate.
#'   Typically a [SurrogateLearner].
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
#' @param epsilon (`numeric(1)`)\cr
#'   Small numeric value used during the log transformation due to numerical stability.
#'   See transformation formula above.
#'
#' @note
#' * The `acq_function$surrogate`, even if already populated, will always be overwritten by the `surrogate`.
#' * The `acq_optimizer$acq_function`, even if already populated, will always be overwritten by `acq_function`.
#' * The `surrogate$archive`, even if already populated, will always be overwritten by the [bbotk::Archive] of the [bbotk::OptimInstanceSingleCrit].
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("jones_1998")`
#' * `r format_bib("snoek_2012")`
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
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "maximize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   surrogate = default_surrogate(instance)
#'
#'   acq_function = acqf("ei")
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search"),
#'     terminator = trm("evals", n_evals = 100))
#'
#'   optimizer = opt("mbo",
#'     loop_function = bayesopt_ego_log,
#'     surrogate = surrogate,
#'     acq_function = acq_function,
#'     acq_optimizer = acq_optimizer)
#'
#'   optimizer$optimize(instance)
#'}
#'}
bayesopt_ego_log = function(
    instance,
    init_design_size = NULL,
    surrogate,
    acq_function,
    acq_optimizer,
    random_interleave_iter = 0L,
    epsilon = 1e-3
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceSingleCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "Surrogate")  # cannot be SurrogateLearner due to EIPS
  assert_r6(acq_function, classes = "AcqFunction")  # FIXME: should explicityly assert acqfs and make sure that codomain tag is handled
  assert_r6(acq_optimizer, classes = "AcqOptimizer")
  assert_int(random_interleave_iter, lower = 0L)
  assert_number(epsilon, lower = 0, upper = 1)

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4L * d

  surrogate$archive = archive
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  surrogate$y_cols = "y_trafo"
  acq_function$surrogate_max_to_min = 1

  if (test_r6(acq_function, classes = "AcqFunctionCB") | test_r6(acq_function, classes = "AcqFunctionMean")) {
    acq_function$codomain$params[[acq_function$id]]$tags = "minimize"
  }

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  } else {
    init_design_size = instance$archive$n_evals
  }

  # loop
  repeat {
    y = instance$archive$data[[instance$archive$cols_y]] * instance$objective_multiplicator[instance$archive$cols_y]
    min_y = min(y) - epsilon * diff(range(y))
    max_y = max(y)
    y_log = log((y - min_y) / (max_y - min_y))
    instance$archive$data[, y_trafo := y_log]

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
      SamplerUnif$new(domain)$sample(1L)$data
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

class(bayesopt_ego_log) = "loop_function"
attr(bayesopt_ego_log, "id") = "bayesopt_ego_log"
attr(bayesopt_ego_log, "label") = "Efficient Global Optimization With Log Transformation"
attr(bayesopt_ego_log, "instance") = "single-crit"
attr(bayesopt_ego_log, "man") = "mlr3mbo::mlr_loop_functions_ego_log"

mlr_loop_functions$add("bayesopt_ego_log", bayesopt_ego_log)

