#' @title Sequential Multicriteria Bayesian Optimization Via ParEGO
#'
#' @description
#' MBO loop function for sequential multicriteria Bayesian optimization via ParEGO.
#' Normally used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstanceMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#' @param surrogate (`NULL` | [SurrogateLearner])\cr
#'   [SurrogateLearner] to be used as a surrogate.
#'   If `NULL` \code{default_surrogate(instance), n_learner = 1} is used.
#'   Points are drawn uniformly at random.
#' @param acq_function (`NULL` | [AcqFunction]).\cr
#'   [AcqFunction] to be used as acquisition function.
#'   If `NULL` [AcqFunctionEI] is used.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   If `NULL` \code{default_acqopt(acqfun)} is used.
#' @param q (`integer(1)`)\cr
#'   Batch size.
#'   Default is `1`.
#' @param s (`integer(1)`)\cr
#'   \eqn{s} in Equation 1 in Knowles (2006).
#'   Determines the total number of possible random weight vectors.
#'   Default is `100`.
#' @param rho (`numeric(1)`)\cr
#'   \eqn{\rho} in Equation 2 in Knowles (2006) scaling the linear part of the augmented Tchebycheff
#'   function.
#'   Default is `0.05`
#' @param random_interleave_iter (`integer(1)`)\cr
#'   Every "random_interleave_iter" iteration (starting after the initial design), a point is
#'   sampled uniformly at random and evaluated (instead of a model based proposal).
#'   For example, if `random_interleave_iter = 2`, random interleaving is performed in the second,
#'   fourth, sixth, ... iteration.
#'   Default is `0`, i.e., no random interleaving is performed at all.
#'
#' @note
#' * If `surrogate` is `NULL` but `acq_function` is given and contains a `$surrogate`, this
#'   [SurrogateLearner] is used.
#' * You can pass a `surrogate` that was not given the [bbotk::Archive] of the
#'   `instance` during initialization.
#'   In this case, the [bbotk::Archive] of the given `instance` is set during execution.
#' * Similarly, you can pass an `acq_function` that was not given the `surrogate` during initialization
#'   and an `acq_optimizer` that was not given the `acq_function`, i.e., delayed initialization is
#'   handled automatically.
#' * The scalarizations of the target variables are stored as the `y_scal` column in the
#'   [bbotk::Archive] of the [bbotk::OptimInstanceMultiCrit].
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' `r format_bib("knowles_2006")`
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
#'   terminator = terminator
#' )
#'
#' bayesopt_parego(instance)
bayesopt_parego = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL,
    q = 1L,
    s = 100L,
    rho = 0.05,
    random_interleave_iter = 0L
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "SurrogateLearner", null.ok = TRUE)
  assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
  assert_int(q, lower = 1L)
  assert_int(s, lower = 1L)
  assert_number(rho, lower = 0, upper = 1)
  assert_int(random_interleave_iter, lower = 0L)

  surrogate = surrogate %??% acq_function$surrogate

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  k = length(archive$cols_y)  # codomain can hold non targets since #08116aa02204980f87c8c08841176ae8f664980a
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4L * d
  if (is.null(surrogate)) surrogate = default_surrogate(instance, n_learner = 1L)
  if (is.null(acq_function)) acq_function = AcqFunctionEI$new()
  if (is.null(acq_optimizer)) acq_optimizer = default_acqopt(acq_function)
  surrogate$archive = archive
  surrogate$y_cols = "y_scal"
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  } else {
    init_design_size = instance$archive$n_evals
  }

  lambdas = calculate_parego_weights(s, k = k)
  qs = seq_len(q)

  # loop
  repeat {
    data = archive$data
    ydt = data[, archive$cols_y, with = FALSE]
    # FIXME: use inplace operations
    ydt = Map("*", ydt, mult_max_to_min(archive$codomain))  # we always assume minimization
    ydt = Map(function(y) (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE)), ydt)  # scale y to [0, 1]

    xdt = map_dtr(qs, function(q) {
      # scalarize y
      lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
      mult = Map("*", ydt, lambda)
      yscal = Reduce("+", mult)
      yscal = do.call(pmax, mult) + rho * yscal  # augmented Tchebycheff function
      data[, y_scal := yscal]  # need to name it yscal due to data.table's behavior

      tryCatch({
        # random interleaving is handled here
        if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
          stop(set_class(list(message = "Random interleaving", call = NULL), classes = c("mbo_error", "random_interleave", "error", "condition")))
        }
        acq_function$surrogate$update()
        acq_function$update()
        acq_optimizer$optimize()
      }, mbo_error = function(mbo_error_condition) {
        lg$info("Proposing a randomly sampled point")
        SamplerUnif$new(domain)$sample(1L)$data
      })
    }, .fill = TRUE)

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

