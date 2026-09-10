#' @title Sequential Single-Objective Bayesian Optimization with Thompson Sampling over Subspaces
#'
#' @include mlr_loop_functions.R
#' @name mlr_loop_functions_thompson
#'
#' @description
#' Loop function for sequential single-objective Bayesian Optimization on a search space that is partitioned into
#' homogeneous subspaces along a single categorical parameter.
#' Normally used inside an [OptimizerMbo].
#'
#' The typical use case is combined algorithm selection and hyperparameter optimization, in which a categorical
#' parameter selects the algorithm, e.g., the branching parameter of an [mlr3pipelines::Graph] that selects the
#' learner.
#' In each iteration after the initial design, one algorithm is sampled via Thompson sampling and the search space is
#' subset to the subspace of that algorithm.
#' The surrogate, the acquisition function, and the acquisition function optimizer are then restricted to this
#' subspace, so that the candidate proposed in this iteration is a configuration of the sampled algorithm.
#'
#' Such a search space is strongly hierarchical, i.e., most of its parameters are only active for a single algorithm.
#' A surrogate fitted on the full search space therefore has to model a highly conditional response surface with many
#' inactive (`NA`) parameters.
#' Restricting the surrogate to the subspace of one algorithm removes most of these dependencies and yields a much
#' lower-dimensional and much better behaved modeling problem.
#'
#' @section Subspaces:
#' The subspaces are derived from `search_space` with [partition_search_space()].
#' By default, every level of `param` becomes a subspace of its own.
#' Use `groups` to put several levels into the same subspace, e.g., to keep related algorithms together.
#'
#' Each evaluation is assigned to exactly one subspace, which is logged into the [bbotk::ArchiveBatch] as an
#' additional `.subspace` column.
#'
#' In each iteration, the surrogate, the acquisition function, and the acquisition function optimizer operate on an
#' [bbotk::ArchiveBatch] that holds the evaluations of the sampled subspace only and whose search space is the
#' subspace.
#' The surrogate is therefore fitted on the evaluations of one algorithm, the acquisition function is optimized over
#' the parameters of that algorithm, and an acquisition function such as [mlr_acqfunctions_ei] improves on the best
#' value observed for that algorithm.
#' Comparing the algorithms with each other is left entirely to the Thompson sampling step.
#'
#' @section Thompson Sampling:
#' Each subspace is an arm of a Beta-Bernoulli bandit.
#' An evaluation counts as a success if its objective value is among the best `top_quantile` fraction of all
#' evaluations in the archive, and as a failure otherwise.
#' The single best evaluation is always a success and evaluations that share the objective value at the boundary are
#' all successes.
#' With \eqn{s_k} successes and \eqn{f_k} failures of subspace \eqn{k}, a value is sampled from the posterior
#' \eqn{\theta_k \sim \mathrm{Beta}(\alpha + s_k, \beta + f_k)} and the subspace with the largest \eqn{\theta_k} is
#' selected.
#'
#' Counting top-quantile hits instead of comparing objective values directly makes the sampling invariant to the
#' scale of the objective and robust to outliers, which matters because the algorithms of such a search space usually
#' differ strongly in how well and how reliably they perform.
#' The successes are recounted on the whole archive in every iteration, so an evaluation that was good early on
#' only stays a success as long as it remains among the best.
#' The prior counts `alpha` and `beta` control how long an algorithm keeps being tried after a streak of failures.
#'
#' A subspace whose parameters are all discrete, i.e., categorical, logical, or bounded integer, has finitely many
#' configurations, e.g., the subspace of a learner without hyperparameters consists of a single configuration.
#' Once all of its configurations have been evaluated, the subspace is exhausted and excluded from the sampling, so
#' that no configuration is evaluated twice.
#' The initial design of such a subspace is capped at the number of its configurations for the same reason.
#' If all subspaces are exhausted, the loop stops before the [bbotk::Terminator] signals termination, because
#' there is nothing left to evaluate.
#'
#' @param instance ([bbotk::OptimInstanceBatchSingleCrit])\cr
#'   The [bbotk::OptimInstanceBatchSingleCrit] to be optimized.
#' @param surrogate ([Surrogate])\cr
#'   [Surrogate] to be used as a surrogate.
#'   Typically a [SurrogateLearner].
#' @param acq_function ([AcqFunction])\cr
#'   [AcqFunction] to be used as acquisition function.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#' @param param (`character(1)`)\cr
#'   Id of the parameter of the search space that selects the algorithm.
#'   Must be a `ParamFct`.
#' @param groups (named `list()` of `character()`)\cr
#'   Groups of levels of `param` that are treated as a single algorithm.
#'   The names of the list are used as the ids of the resulting subspaces.
#'   The groups must be disjoint and must cover all levels of `param`.
#'   Default is `NULL`, i.e., every level of `param` becomes a subspace of its own.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design *per subspace*.
#'   If `NULL` and the [bbotk::ArchiveBatch] contains no evaluations, \code{4 * d} is used with \code{d} being the
#'   dimensionality of the respective subspace.
#'   Points are sampled uniformly at random within each subspace.
#' @param top_quantile (`numeric(1)`)\cr
#'   Fraction of the evaluations in the archive that count as successes, i.e., an evaluation is a success if its
#'   objective value is among the best `top_quantile` fraction of all evaluations.
#'   Default is `0.1`, i.e., the top decile.
#' @param alpha (`numeric(1)`)\cr
#'   \eqn{\alpha} of the Beta prior, i.e., the prior number of successes of every subspace.
#'   Default is `1`.
#' @param beta (`numeric(1)`)\cr
#'   \eqn{\beta} of the Beta prior, i.e., the prior number of failures of every subspace.
#'   Default is `1`.
#' @param random_interleave_iter (`integer(1)`)\cr
#'   Every `random_interleave_iter` iteration (starting after the initial design), a point is
#'   sampled uniformly at random from the sampled subspace and evaluated (instead of a model based proposal).
#'   For example, if `random_interleave_iter = 2`, random interleaving is performed in the second,
#'   fourth, sixth, ... iteration.
#'   Default is `0`, i.e., no random interleaving is performed at all.
#'
#' @note
#' * The `acq_function$surrogate`, even if already populated, will always be overwritten by the `surrogate`.
#' * The `acq_optimizer$acq_function`, even if already populated, will always be overwritten by `acq_function`.
#' * The `surrogate$archive` and `surrogate$cols_x`, even if already populated, will always be overwritten by the
#'   [bbotk::ArchiveBatch] and the parameters of the subspace sampled in the respective iteration.
#'   After termination, they are reset to the [bbotk::ArchiveBatch] of the
#'   [bbotk::OptimInstanceBatchSingleCrit] and the full search space, so that a final update of the surrogate,
#'   e.g. the one performed by [OptimizerMbo], is fitted on all evaluations across subspaces.
#' * A subspace that groups several levels of `param` can still contain parameters that are inactive for some of
#'   its levels.
#'   The surrogate learner must be able to handle the resulting missing values, as for the full search space.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("thompson_1933")`
#' * `r format_bib("thornton_2013")`
#'
#' @family Loop Function
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("ranger")) {
#'
#'   library(bbotk)
#'   library(data.table)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y = if (xs$learner == "a") xs$a_x^2 else abs(xs$b_x))
#'   }
#'   domain = ps(
#'     learner = p_fct(c("a", "b")),
#'     a_x = p_dbl(lower = -10, upper = 10, depends = learner == "a"),
#'     b_x = p_dbl(lower = -10, upper = 10, depends = learner == "b")
#'   )
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 20))
#'
#'   optimizer = opt("mbo",
#'     loop_function = bayesopt_thompson,
#'     args = list(param = "learner", init_design_size = 4),
#'     surrogate = default_surrogate(instance, force_random_forest = TRUE),
#'     acq_function = acqf("ei"),
#'     acq_optimizer = acqo(
#'       optimizer = opt("random_search", batch_size = 100),
#'       terminator = trm("evals", n_evals = 100)))
#'
#'   optimizer$optimize(instance)
#'
#'   instance$archive$data[, .N, by = ".subspace"]
#' }
#' }
bayesopt_thompson = function(
  instance,
  surrogate,
  acq_function,
  acq_optimizer,
  param,
  groups = NULL,
  init_design_size = NULL,
  top_quantile = 0.1,
  alpha = 1,
  beta = 1,
  random_interleave_iter = 0L
) {
  # assertions
  assert_r6(instance, "OptimInstanceBatchSingleCrit")
  assert_r6(surrogate, classes = "Surrogate")
  assert_r6(acq_function, classes = "AcqFunction")
  assert_r6(acq_optimizer, classes = "AcqOptimizer")
  search_space = instance$search_space
  assert_choice(param, choices = search_space$ids())
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_number(top_quantile, lower = 0, upper = 1)
  assert_number(alpha, lower = 0, finite = TRUE)
  assert_number(beta, lower = 0, finite = TRUE)
  assert_int(random_interleave_iter, lower = 0L)

  # every level of the algorithm parameter is a subspace of its own unless the levels are grouped explicitly
  subspaces = partition_search_space(search_space, param = param, groups = groups)
  subspace_ids = names(subspaces)

  cols_x = instance$archive$cols_x
  col_y = instance$archive$cols_y
  na_x = na_values(search_space)
  # a subspace with finitely many configurations can be exhausted, so its number of configurations is needed
  n_configurations = map_dbl(subspaces, function(subspace) nrow(subspace_grid(subspace)) %??% Inf)
  # the bandit counts top-quantile hits, so the objective values are always oriented towards minimization
  y_mult = mult_max_to_min(instance$archive$codomain)[[col_y]]

  # initial design: every subspace receives its own design, so that the bandit starts from an observation per arm
  init_design_size = if (instance$archive$n_evals == 0L) {
    design = map_dtr(
      subspace_ids,
      function(subspace_id) {
        subspace = subspaces[[subspace_id]]
        n = init_design_size %??% (4L * subspace$length)
        pad_xdt(generate_design_subspace(subspace, n = n), cols_x = cols_x, na_values = na_x)
      },
      .fill = TRUE
    )
    instance$eval_batch(design)
    nrow(design)
  } else {
    # a user-supplied initial design already in the archive leaves init_design_size NULL,
    # which would otherwise silently disable random interleaving
    instance$archive$n_evals
  }

  # actual loop
  repeat {
    data = instance$archive$data
    set(data, j = ".subspace", value = assign_subspaces(subspaces, data))

    # exhausted subspaces have nothing left to evaluate and are excluded from the sampling
    exhausted = map_lgl(subspace_ids, function(subspace_id) {
      subspace_exhausted(
        subspaces[[subspace_id]],
        data[which(data[[".subspace"]] == subspace_id)],
        n_configurations[[subspace_id]]
      )
    })
    if (all(exhausted)) {
      lg$info("All subspaces are exhausted, stopping the optimization")
      break
    }

    subspace_id = thompson_sample_subspace(
      subspace_ids[!exhausted],
      subspace = data[[".subspace"]],
      y = data[[col_y]] * y_mult,
      top_quantile = top_quantile,
      alpha = alpha,
      beta = beta
    )
    lg$debug("Thompson sampling selected subspace '%s'", subspace_id)

    # the surrogate, the acquisition function and its optimizer only ever see the evaluations of the sampled
    # subspace, so that the modeling problem, the incumbent, an output transformation and the acquisition function
    # optimization are all restricted to the algorithm of this iteration
    subspace = subspaces[[subspace_id]]
    archive = ArchiveBatch$new(
      search_space = subspace,
      codomain = instance$archive$codomain,
      check_values = FALSE
    )
    archive$data = data[which(data[[".subspace"]] == subspace_id)]
    surrogate$archive = archive
    surrogate$cols_x = subspace$ids()
    # assigning the surrogate derives the domain of the acquisition function from the subspace
    acq_function$surrogate = surrogate
    acq_optimizer$acq_function = acq_function

    xdt = tryCatch(
      {
        # random interleaving is handled here
        if (isTRUE((instance$archive$n_evals - init_design_size + 1L) %% random_interleave_iter == 0)) {
          error_random_interleave("Random interleaving")
        }

        acq_function$surrogate$update()
        acq_function$update()
        acq_optimizer$optimize()
      },
      Mlr3ErrorMboRandomInterleave = function(cond) {
        lg$info("Random interleaving triggered, proposing a randomly sampled point of subspace '%s'", subspace_id)
        generate_design_random(subspace, n = 1L)$data
      },
      Mlr3ErrorMbo = function(cond) {
        lg$warn("Caught the following error: %s", cond$message)
        lg$info("Proposing a randomly sampled point of subspace '%s'", subspace_id)
        generate_design_random(subspace, n = 1L)$data
      }
    )

    instance$eval_batch(pad_xdt(xdt, cols_x = cols_x, na_values = na_x))
    if (instance$is_terminated) break
  }

  # the point evaluated last is only added to the archive after the final assignment within the loop
  data = instance$archive$data
  set(data, j = ".subspace", value = assign_subspaces(subspaces, data))

  # leave the surrogate pointing at the whole archive again, so that a final update after the loop, e.g. the one
  # of OptimizerMbo, is performed on all evaluations across subspaces
  surrogate$archive = instance$archive
  surrogate$cols_x = cols_x

  invisible(instance)
}

class(bayesopt_thompson) = "loop_function"
attr(bayesopt_thompson, "id") = "bayesopt_thompson"
attr(bayesopt_thompson, "label") = "Thompson Sampling over Subspaces"
attr(bayesopt_thompson, "instance") = "single-crit"
attr(bayesopt_thompson, "man") = "mlr3mbo::mlr_loop_functions_thompson"

mlr_loop_functions$add("bayesopt_thompson", bayesopt_thompson)

# id of the subspace every row of `data` belongs to; the subspaces are disjoint, so the first match is the only one
assign_subspaces = function(subspaces, data) {
  assignment = rep(NA_character_, nrow(data))
  for (subspace_id in names(subspaces)) {
    assignment[is.na(assignment) & subspace_contains(subspaces[[subspace_id]], data)] = subspace_id
  }
  assignment
}

# Beta-Bernoulli Thompson sampling over the subspaces; an evaluation counts as a success if its outcome is a
# top-quantile hit, so that the sampling is invariant to the scale of the objective
thompson_sample_subspace = function(subspace_ids, subspace, y, top_quantile, alpha, beta) {
  success = top_quantile_hits(y, top_quantile)

  theta = map_dbl(subspace_ids, function(subspace_id) {
    evaluated = which(subspace == subspace_id)
    successes = sum(success[evaluated])
    rbeta(1L, alpha + successes, beta + length(evaluated) - successes)
  })

  subspace_ids[which.max(theta)]
}

# whether each outcome (oriented towards minimization) is among the best `top_quantile` fraction of all outcomes;
# the single best outcome always is, ties at the boundary all are, and failed evaluations (`NA`) never are
top_quantile_hits = function(y, top_quantile) {
  observed = !is.na(y)
  if (!any(observed)) {
    return(observed)
  }
  n_top = max(1L, ceiling(top_quantile * sum(observed)))
  threshold = sort(y[observed])[n_top]
  observed & y <= threshold
}

# a point of a subspace only holds the parameters of that subspace, but the archive expects all parameters of the
# search space; the remaining ones are inactive and therefore `NA`
pad_xdt = function(xdt, cols_x, na_values) {
  missing = setdiff(cols_x, names(xdt))
  for (id in missing) {
    set(xdt, j = id, value = na_values[[id]])
  }
  setcolorder(xdt, cols_x)[]
}
