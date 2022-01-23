#' @title Sequential Multicriteria Bayesian Optimization via Expected Hypervolume Improvement
#'
#' @description
#' MBO loop function for sequential multicriteria Bayesian optimization via expected hypervolume improvement.
#' Normally used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstanceMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param surrogate (`NULL` | [SurrogateLearners])\cr
#'   [SurrogateLearners] to be used as a surrogate.
#'   If `NULL` \code{default_surrogate(instance)} is used.
#' @param acq_function (`NULL` | [AcqFunctionEHVI]).\cr
#'   [AcqFunctionEHVI] to be used as acquisition function.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'
#' @note
#' * If `surrogate` is `NULL` but `acq_function` contains a `$surrogate`, this [SurrogateLearners] is used.
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
#' @family Loop Function
#' @export
#' @examples
#' library(bbotk)
#' library(paradox)
#' library(mlr3learners)
#'
#' fun = function(xs) {
#'   list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#' }
#' domain = ps(x = p_dbl(lower = -10, upper = 10))
#' codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#' objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#' terminator = trm("evals", n_evals = 5)
#'
#' instance = OptimInstanceMultiCrit$new(
#'   objective = objective,
#'   terminator = terminator,
#' )
#'
#' bayesopt_ehvi(instance)
bayesopt_ehvi = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL
  ) {

  # FIXME: think about a general loop function, this doesn't do much different than ego/mego

  # assertions and defaults
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "SurrogateLearners", null.ok = TRUE)
  assert_r6(acq_function, classes = "AcqFunctionEHVI", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)

  surrogate = surrogate %??% acq_function$surrogate

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4 * d
  if (is.null(surrogate)) surrogate = default_surrogate(instance)
  if (is.null(acq_function)) acq_function = AcqFunctionEHVI$new()
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

