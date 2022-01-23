#' @title Sequential Singlecriteria Bayesian Optimization
#'
#' @description
#' MBO loop function for sequential singlecriteria Bayesian optimization.
#' Normally used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit])\cr
#'   The [bbotk::OptimInstanceSingleCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param surrogate (`NULL` | [Surrogate])\cr
#'   [Surrogate] to be used as a surrogate.
#'   Typically a [SurrogateLearner].
#'   If `NULL` \code{default_surrogate(instance)} is used.
#' @param acq_function (`NULL` | [AcqFunction]).\cr
#'   [AcqFunction] to be used as acquisition function.
#'   If `NULL` \code{default_acqfun(instance)} is used.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   If `NULL` \code{default_acqopt(acqfun)} is used.
#'
#' @note
#' * If `surrogate` is `NULL` but `acq_function` is given and contains a `$surrogate`, this
#'   [Surrogate] is used.
#' * You can pass a `surrogate` that was not given the [bbotk::Archive] of the
#'   `instance` during initialization.
#'   In this case, the [bbotk::Archive] of the given `instance` is set during execution.
#' * Similarly, you can pass an `acq_function` that was not given the `surrogate` during initialization
#'   and an `acq_optimizer` that was not given the `acq_function`, i.e., delayed initialization is
#'   handled automatically.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("jones_1998")`
#' * `r format_bib("snoek_2012")`
#' @family Loop Function
#' @export
#' @examples
#' library(bbotk)
#' library(paradox)
#' library(mlr3learners)
#'
#' # expected improvement
#' objective = ObjectiveRFun$new(
#'   fun = function(xs) list(y = xs$x ^ 2),
#'   domain = ps(x = p_dbl(lower = -5, upper = 5)),
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#' )
#'
#' terminator = trm("evals", n_evals = 5)
#'
#' instance = OptimInstanceSingleCrit$new(
#'   objective = objective,
#'   terminator = terminator
#' )
#'
#' bayesopt_ego(instance)
#'
#' # expected improvement per second
#' objective = ObjectiveRFun$new(
#'   fun = function(xs) list(y = xs$x ^ 2, time = abs(xs$x)),
#'   domain = ps(x = p_dbl(lower = -5, upper = 5)),
#'   codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time")),
#'   id = "xsq"
#' )
#'
#' terminator = trm("evals", n_evals = 5)
#'
#' instance = OptimInstanceSingleCrit$new(
#'   objective = objective,
#'   terminator = terminator
#' )
#'
#' surrogate = default_surrogate(instance, n_learner = 2L)
#' surrogate$y_cols = c("y", "time")
#'
#' acq_function = AcqFunctionEIPS$new()
#'
#' bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function)
bayesopt_ego = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceSingleCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)  # cannot be SurrogateLearner due to EIPS
  assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)

  surrogate = surrogate %??% acq_function$surrogate

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4 * d
  if (is.null(surrogate)) surrogate = default_surrogate(instance)
  if (is.null(acq_function)) acq_function = default_acqfun(instance)
  if (is.null(acq_optimizer)) acq_optimizer = default_acqopt(acq_function)
  surrogate$archive = archive
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  }

  # loop
  repeat {
    xdt = tryCatch({
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      lg$info("Proposing a randomly sampled point")
      SamplerUnif$new(domain)$sample(1L)$data
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

