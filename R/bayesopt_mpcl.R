#' @title Sequential Singlecriteria Bayesian Optimization Via Multipoint Constant Liar
#'
#' @description
#' MBO loop function for sequential singlecriteria Bayesian optimization via multipoint constant liar.
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
#' @param acq_function (`NULL` | [AcqFunction]).
#'   [AcqFunction] to be used as acquisition function.
#'   If `NULL` \code{default_acqfun(instance)} is used.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   If `NULL` \code{default_acqopt(acqfun)} is used.
#' @param q (`integer(1)`)\cr
#'   Batch size > `1`.
#'   Default is `2`.
#' @param liar (`function`)\cr
#'   Any function accepting a numeric vector as input and returning a single numeric output.
#'   Default is `mean`.
#' @param random_interleave_iter (`integer(1)`)\cr
#'   Every "random_interleave_iter" iteration (starting after the initial design), a point is
#'   sampled uniformly at random and evaluated (instead of a model based proposal).
#'   For example, if `random_interleave_iter = 2`, random interleaving is performed in the second,
#'   fourth, sixth, ... iteration.
#'   Default is `0`, i.e., no random interleaving is performed at all.
#'
#' @note
#' * If `surrogate` is `NULL` but `acq_function` is given and contains a `$surrogate`, this
#' [Surrogate] is used.
#' * You can pass a `surrogate` that was not given the [bbotk::Archive] of the
#' `instance` during initialization.
#' In this case, the [bbotk::Archive] of the given `instance` is set during execution.
#' * Similarly, you can pass an `acq_function` that was not given the `surrogate` during initialization
#' and an `acq_optimizer` that was not given the `acq_function`, i.e., delayed initialization is
#' handled automatically.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' `r format_bib("wang_2020")`
#' @family Loop Function
#' @export
#' @examples
#' library(bbotk)
#' library(paradox)
#' library(mlr3learners)
#'
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
#' bayesopt_mpcl(instance)
bayesopt_mpcl = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL,
    q = 2L,
    liar = mean,
    random_interleave_iter = 0L
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceSingleCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)  # cannot be SurrogateLearner due to EIPS
  assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
  assert_int(q, lower = 2L)
  assert_function(liar)
  assert_int(random_interleave_iter, lower = 0L)

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

  lie = data.table()

  # loop
  repeat {
    # normal ego proposal with error catching
    xdt = tryCatch({
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      #lg$info("Proposing a randomly sampled point") no logging because we do not evaluate this point
      SamplerUnif$new(domain)$sample(1L)$data
    })

    # prepare lie objects
    tmp_archive = archive$clone(deep = TRUE)
    acq_function$surrogate$archive = tmp_archive
    lie[, archive$cols_y := liar(archive$data[[archive$cols_y]])]  # FIXME: assert output of liar
    xdt_new = xdt

    # obtain proposals using fake archive with lie, also with error catching
    xdt = map_dtr(seq_len(q)[-1L], function(i) {  # this is save because q is asserted >= 2)
      tryCatch({
        # add lie instead of true eval
        tmp_archive$add_evals(xdt = xdt_new, xss_trafoed = transform_xdt_to_xss(xdt_new, tmp_archive$search_space), ydt = lie)

        # random interleaving is handled here
        if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
          stop(set_class(list(message = "Random interleaving", call = NULL), classes = c("mbo_error", "random_interleave", "error", "condition")))
        }

        # update all objects with lie
        acq_function$surrogate$update()
        acq_function$update()
        acq_optimizer$optimize()
      }, mbo_error = function(mbo_error_condition) {
        lg$info("Proposing a randomly sampled point")
        SamplerUnif$new(domain)$sample(1L)$data
      })
    })

    acq_function$surrogate$archive = archive

    instance$eval_batch(xdt)

    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

