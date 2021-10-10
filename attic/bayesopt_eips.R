#' @title Sequential Single Objective Bayesian Optimization Using Expected Improvement per Second
#'
#' @description
#' MBO loop function for sequential single objective Bayesian optimization.
#' Uses Expected Improvement per Second as [AcqFunction].
#' Normaly used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceSingleCrit])\cr
#'   The [bbotk::OptimInstanceSingleCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#' @param surrogate (`NULL` | [SurogateLearners])\cr
#'   [SurrogateLearner] to be used as a surrogate.
#'   If `NULL` \code{default_surrogate(instance)} is used.
#' @param acq_function (`NULL` | [AcqFunction]).
#'   [AcqFunction] to be used as an acquisition function.
#'   If `NULL` \code{default_acqfun(instance)} is used.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   If `NULL` \code{default_acqopt(acqfun)} is used.
#'
#' @note
#' If `surrogate` is `NULL` but `acq_function` is given and contains a `$surrogate`, this
#' [SurrogateLearner] is used.\cr
#' You can pass a `surrogate` that was not given the [bbotk::Archive] of the
#' `instance` during initialization.
#' In this case, the [bbotk::Archive] of the given `instance` is set during execution.\cr
#' Similarly, you can pass an `acq_function` that was not given the `surrogate` during initialization
#' and an `acq_optimizer` that was not given the `acq_function`, i.e., delayed initialization is
#' handled automatically.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' `r format_bib("jones_1998")`
#' @family Loop Function
#' @export
#' @examples
#' objective = ObjectiveRFun$new(
#'   fun = function(xs) list(y = xs$x ^ 2, time = abs(xs$x)),
#'   domain = ps(x = p_dbl(lower = -5, upper = 5)),
#'   codomain = ps(y = p_dbl(tags = "minimize"), time = p_dbl(tags = "time")),
#'   id = "xsq"
#' )
#'
#' terminator = trm("evals", n_evals = 20)
#'
#' instance = OptimInstanceSingleCrit$new(
#'   objective = objective,
#'   terminator = terminator
#' )
#'
#' bayesopt_eips(instance)
#' instance
bayesopt_eips = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceSingleCrit")
  if (length(instance$archive$codomain$ids(tags = "time")) != 1L) {
    stop("Need exactly one parameter in the codomain tagged as 'time'.")
  }
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "SurrogateLearners", null.ok = TRUE)
  assert_r6(acq_function, classes = "AcqFunctionEIPS", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)

  surrogate = surrogate %??% acq_function$surrogate

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  y_cols = c(archive$cols_y, instance$archive$codomain$ids(tags = "time"))
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4 * d
  if (is.null(surrogate)) surrogate = default_surrogate(instance, n_learner = 2L)
  if (is.null(acq_function)) acq_function = AcqFunctionEIPS$new()
  if (is.null(acq_optimizer)) acq_optimizer = default_acqopt(acq_function)
  surrogate$archive = archive
  surrogate$y_cols = y_cols
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

