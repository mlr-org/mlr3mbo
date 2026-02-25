#' @title Single-Objective Bayesian Optimization via Multipoint Constant Liar
#'
#' @include mlr_loop_functions.R
#' @name mlr_loop_functions_mpcl
#'
#' @description
#' Loop function for single-objective Bayesian Optimization via multipoint constant liar.
#' Normally used inside an [OptimizerMbo].
#'
#' In each iteration after the initial design, the surrogate and acquisition function are updated.
#' The acquisition function is then optimized, to find a candidate but instead of evaluating this candidate, the
#' objective function value is obtained by applying the `liar` function to all previously obtained objective function values.
#' This is repeated `q - 1` times to obtain a total of `q` candidates that are then evaluated in a single batch.
#'
#' @param instance ([bbotk::OptimInstanceBatchSingleCrit])\cr
#'   The [bbotk::OptimInstanceBatchSingleCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` and the [bbotk::ArchiveBatch] contains no evaluations, \code{4 * d} is used with \code{d} being the
#'   dimensionality of the search space.
#'   Points are generated via a Sobol sequence.
#' @param surrogate ([Surrogate])\cr
#'   [Surrogate] to be used as a surrogate.
#'   Typically a [SurrogateLearner].
#' @param acq_function ([AcqFunction])\cr
#'   [AcqFunction] to be used as acquisition function.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#' @param q (`integer(1)`)\cr
#'   Batch size > `1`.
#'   Default is `2`.
#' @param liar (`function`)\cr
#'   Any function accepting a numeric vector as input and returning a single numeric output.
#'   Default is `mean`. Other sensible functions include `min` (or `max`, depending on the optimization direction).
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
#' * The `surrogate$archive`, even if already populated, will always be overwritten by the [bbotk::ArchiveBatch] of the [bbotk::OptimInstanceBatchSingleCrit].
#' * To make use of parallel evaluations in the case of `q > 1, the objective
#'   function of the [bbotk::OptimInstanceBatchSingleCrit] must be implemented accordingly.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("ginsbourger_2008")`
#' * `r format_bib("wang_2020")`
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
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 7))
#'
#'   surrogate = default_surrogate(instance)
#'
#'   acq_function = acqf("ei")
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search", batch_size = 100),
#'     terminator = trm("evals", n_evals = 100))
#'
#'   optimizer = opt("mbo",
#'     loop_function = bayesopt_mpcl,
#'     surrogate = surrogate,
#'     acq_function = acq_function,
#'     acq_optimizer = acq_optimizer,
#'     args = list(q = 3))
#'
#'   optimizer$optimize(instance)
#' }
#' }
bayesopt_mpcl = function(
    instance,
    surrogate,
    acq_function,
    acq_optimizer,
    init_design_size = NULL,
    q = 2L,
    liar = mean,
    random_interleave_iter = 0L
  ) {

  # assertions
  assert_r6(instance, "OptimInstanceBatchSingleCrit")
  assert_r6(surrogate, classes = "Surrogate")  # cannot be SurrogateLearner due to EIPS
  assert_r6(acq_function, classes = "AcqFunction")
  assert_r6(acq_optimizer, classes = "AcqOptimizer")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_int(q, lower = 2L)
  assert_function(liar)
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
  acq_function$surrogate = surrogate
  acq_optimizer$acq_function = acq_function

  lie = data.table()

  # actual loop
  repeat {
    # normal ego proposal with error catching
    xdt = tryCatch({
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$optimize()
    }, Mlr3ErrorMbo = function(cond) {
      #lg$info("Proposing a randomly sampled point") no logging because we do not evaluate this point
      generate_design_random(search_space, n = 1L)$data
    })

    # prepare lie objects
    tmp_archive = instance$archive$clone(deep = TRUE)
    acq_function$surrogate$archive = tmp_archive
    lie[, instance$archive$cols_y := liar(instance$archive$data[[instance$archive$cols_y]])]  # FIXME: assert output of liar
    xdt_new = xdt

    # obtain proposals using fake archive with lie, also with error catching
    for (i in seq_len(q)[-1L]) { # this is save because q is asserted >= 2
      xdt_new = tryCatch({
        # add lie instead of true eval
        tmp_archive$add_evals(xdt = xdt_new, xss_trafoed = transform_xdt_to_xss(xdt_new, tmp_archive$search_space), ydt = lie)

        # random interleaving is handled here
        if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
          error_random_interleave("Random interleaving")
        }

        # update all objects with lie
        acq_function$surrogate$update()
        acq_function$update()
        acq_optimizer$optimize()
      }, Mlr3ErrorMboRandomInterleave = function(cond) {
        lg$info("Random interleaving triggered, proposing a randomly sampled point")
        generate_design_random(search_space, n = 1L)$data
      }, Mlr3ErrorMbo = function(cond) {
        lg$warn("Caught the following error: %s", cond$message)
        lg$info("Proposing a randomly sampled point")
        generate_design_random(search_space, n = 1L)$data
      })
      xdt = rbind(xdt, xdt_new)
    }

    acq_function$surrogate$archive = instance$archive

    instance$eval_batch(xdt)

    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

class(bayesopt_mpcl) = "loop_function"
attr(bayesopt_mpcl, "id") = "bayesopt_mpcl"
attr(bayesopt_mpcl, "label") = "Multipoint Constant Liar"
attr(bayesopt_mpcl, "instance") = "single-crit"
attr(bayesopt_mpcl, "man") = "mlr3mbo::mlr_loop_functions_mpcl"

mlr_loop_functions$add("bayesopt_mpcl", bayesopt_mpcl)

