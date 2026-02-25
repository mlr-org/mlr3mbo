#' @title Multi-Objective Bayesian Optimization via ParEGO
#'
#' @include mlr_loop_functions.R
#' @name mlr_loop_functions_parego
#'
#' @description
#' Loop function for multi-objective Bayesian Optimization via ParEGO.
#' Normally used inside an [OptimizerMbo].
#'
#' In each iteration after the initial design, the observed objective function values are normalized and `q` candidates are
#' obtained by scalarizing these values via the augmented Tchebycheff function, updating the surrogate with respect to
#' these scalarized values and optimizing the acquisition function.
#'
#' @param instance ([bbotk::OptimInstanceBatchMultiCrit])\cr
#'   The [bbotk::OptimInstanceBatchMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` and the [bbotk::ArchiveBatch] contains no evaluations, \code{4 * d} is used with \code{d} being the
#'   dimensionality of the search space.
#'   Points are generated via a Sobol sequence.
#' @param surrogate ([SurrogateLearner])\cr
#'   [SurrogateLearner] to be used as a surrogate.
#' @param acq_function ([AcqFunction])\cr
#'   [AcqFunction] to be used as acquisition function.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#' @param q (`integer(1)`)\cr
#'   Batch size, i.e., the number of candidates to be obtained for a single batch.
#'   Default is `1`.
#' @param s (`integer(1)`)\cr
#'   \eqn{s} in Equation 1 in Knowles (2006).
#'   Determines the total number of possible random weight vectors.
#'   Default is `100`.
#' @param rho (`numeric(1)`)\cr
#'   \eqn{\rho} in Equation 2 in Knowles (2006) scaling the linear part of the augmented Tchebycheff function.
#'   Default is `0.05`
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
#' * The `surrogate$archive`, even if already populated, will always be overwritten by the [bbotk::ArchiveBatch] of the [bbotk::OptimInstanceBatchMultiCrit].
#' * The scalarizations of the objective function values are stored as the `y_scal` column in the
#'   [bbotk::ArchiveBatch] of the [bbotk::OptimInstanceBatchMultiCrit].
#' * To make use of parallel evaluations in the case of `q > 1, the objective
#'   function of the [bbotk::OptimInstanceBatchMultiCrit] must be implemented accordingly.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("knowles_2006")`
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
#'   instance = OptimInstanceBatchMultiCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   surrogate = default_surrogate(instance, n_learner = 1)
#'
#'   acq_function = acqf("ei")
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search", batch_size = 100),
#'     terminator = trm("evals", n_evals = 100))
#'
#'   optimizer = opt("mbo",
#'     loop_function = bayesopt_parego,
#'     surrogate = surrogate,
#'     acq_function = acq_function,
#'     acq_optimizer = acq_optimizer)
#'
#'   optimizer$optimize(instance)
#' }
#' }
bayesopt_parego = function(
    instance,
    surrogate,
    acq_function,
    acq_optimizer,
    init_design_size = NULL,
    q = 1L,
    s = 100L,
    rho = 0.05,
    random_interleave_iter = 0L
  ) {

  # assertions
  assert_r6(instance, "OptimInstanceBatchMultiCrit")
  assert_r6(surrogate, classes = "SurrogateLearner")
  assert_r6(acq_function, classes = "AcqFunction")
  assert_r6(acq_optimizer, classes = "AcqOptimizer")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_int(q, lower = 1L)
  assert_int(s, lower = 1L)
  assert_number(rho, lower = 0, upper = 1)
  assert_int(random_interleave_iter, lower = 0L)

  # initial design
  search_space = instance$search_space
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) {
    init_design_size = 4L * search_space$length
  }
  if (!is.null(init_design_size) && instance$archive$n_evals == 0L) {
    design = generate_design_sobol(search_space, n = init_design_size)$data
    instance$eval_batch(design)
  }

  # completing initialization
  surrogate$archive = instance$archive
  surrogate$cols_y = "y_scal"
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  k = length(instance$archive$cols_y)  # codomain can hold non targets since #08116aa02204980f87c8c08841176ae8f664980a
  lambdas = calculate_parego_weights(s, k = k)
  qs = seq_len(q)

  # actual loop
  repeat {
    data = instance$archive$data
    ydt = data[, instance$archive$cols_y, with = FALSE]
    ydt = Map("*", ydt, mult_max_to_min(instance$archive$codomain))  # we always assume minimization
    ydt = Map(function(y) (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE)), ydt)  # scale y to [0, 1]

    xdt = map_dtr(qs, function(q) {
      # scalarize y
      lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
      mult = Map("*", ydt, lambda)
      y_scal = Reduce("+", mult)
      y_scal = do.call(pmax, mult) + rho * y_scal  # augmented Tchebycheff function
      set(data, j = "y_scal", value = y_scal)

      tryCatch({
        # random interleaving is handled here
        if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
          error_random_interleave("Random interleaving")
        }
        acq_function$surrogate$update()
        acq_function$update()
        acq_optimizer$optimize()
      }, Mlr3ErrorMboRandomInterleave = function(cond) {
        lg$info("Random interleaving triggered, proposing a randomly sampled point")
        generate_design_random(search_space, n = 1L)$data
      }, Mlr3ErrorMbo = function(cond) {
        lg$warn("Catched the following error: %s", cond$message)
        lg$info("Proposing a randomly sampled point")
        generate_design_random(search_space, n = 1L)$data
      })
    }, .fill = TRUE)

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

class(bayesopt_parego) = "loop_function"
attr(bayesopt_parego, "id") = "bayesopt_parego"
attr(bayesopt_parego, "label") = "ParEGO"
attr(bayesopt_parego, "instance") = "multi-crit"
attr(bayesopt_parego, "man") = "mlr3mbo::mlr_loop_functions_parego"

mlr_loop_functions$add("bayesopt_parego", bayesopt_parego)

