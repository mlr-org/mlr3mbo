#' @title Asynchronous Decentralized Bayesian Optimization with Thompson Sampling over Subspaces
#'
#' @include OptimizerADBO.R
#' @name mlr_optimizers_adbo_thompson
#'
#' @description
#' `OptimizerADBOThompson` runs Asynchronous Decentralized Bayesian Optimization (ADBO) on a hierarchical search space
#' and samples the subspace to work on via Thompson sampling.
#' The typical use case is combined algorithm selection and hyperparameter optimization,
#' e.g., an AutoML search space in which the branching parameter of an [mlr3pipelines::Graph] selects the learner.
#'
#' A surrogate fitted on the full search space has to model a highly conditional response surface with many inactive
#' (`NA`) parameters.
#' `OptimizerADBOThompson` therefore splits the search space into one subspace per level of the `branch` parameter.
#' In each iteration, a worker samples one subspace and fits the [SurrogateLearner] on the evaluations and the
#' parameters of that subspace only, which yields a much lower-dimensional and much better behaved modeling problem.
#'
#' `OptimizerADBOThompson` is considered an experimental feature and the API might be subject to changes.
#' Currently, only single-objective optimization is supported.
#'
#' @section Subspaces:
#' The `branch` parameter is the id of a categorical parameter of the search space.
#' Every level of it defines a subspace, see [partition_search_space()], and the subspaces are named like the levels.
#' The subspace of an evaluation is the value of the `branch` parameter in the [bbotk::ArchiveAsync].
#'
#' @section Compute Profiles:
#' By default, every worker samples among all subspaces.
#' When different parts of the search space require different hardware, e.g., learners that can only be trained on a
#' GPU versus learners that are trained on CPU cores, `subspace_profiles` maps every subspace to the
#' [compute profile](https://mirai.r-lib.org/articles/mirai.html#scoped-profiles) of \CRANpkg{mirai} it runs on.
#' A worker then only samples among the subspaces of the profile it runs on.
#' Several subspaces may share a profile and with one subspace per profile every worker is permanently assigned to
#' one subspace.
#'
#' The daemons of every profile must be created beforehand and the number of workers per profile is set with the
#' `profiles` parameter or with [rush::rush_plan()].
#' Here, the `"tabpfn"` subspace runs on the single GPU while the `"ranger"` and `"xgboost"` subspaces share the seven
#' workers on the CPU cores.
#'
#' ```
#' mirai::daemons(1, .compute = "gpu")
#' mirai::daemons(7, .compute = "cpu")
#'
#' opt("adbo_thompson",
#'   branch = "learner",
#'   subspace_profiles = c(tabpfn = "gpu", ranger = "cpu", xgboost = "cpu"),
#'   profiles = c(gpu = 1L, cpu = 7L))
#' ```
#'
#' A worker that runs on no compute profile, e.g., when the workers are started with `n_workers` or in the debug mode
#' of \CRANpkg{bbotk}, samples among all subspaces.
#'
#' @section Initial Design:
#' Every subspace receives an initial design of `design_size` randomly sampled points.
#' The design is not generated upfront.
#' Instead, a worker proposes a random point of the subspace with the fewest points among its subspaces,
#' as long as one of them holds fewer than `design_size` points, counting queued, running, finished, and failed points.
#' The subspaces are therefore covered evenly even if the optimization terminates during the initial design.
#' Because the workers do not coordinate, a subspace may receive a few more than `design_size` random points.
#' For the same reason, workers that propose at the same time may evaluate the same configuration of a subspace with
#' finitely many configurations more than once, see section Thompson Sampling.
#'
#' Points passed as `initial_design` are pushed to the shared queue, evaluated by any worker regardless of its
#' compute profile, and count towards `design_size` of the subspace they belong to.
#' The `design_function` parameter is not used.
#'
#' @section Thompson Sampling:
#' Each subspace is an arm of a Beta-Bernoulli bandit.
#' An evaluation counts as a success if its objective value is among the best `top_quantile` fraction of all finished
#' evaluations, and as a failure otherwise.
#' The single best evaluation is always a success and evaluations
#' that share the objective value at the boundary are all successes.
#' With \eqn{s_k} successes and \eqn{f_k} failures of subspace \eqn{k}, a value is sampled from the posterior
#' \eqn{\theta_k \sim \mathrm{Beta}(\alpha + s_k, \beta + f_k)}
#' and the subspace with the largest \eqn{\theta_k} is selected.
#'
#' A worker only samples among its own subspaces,
#' but the successes and failures are counted on all finished evaluations of the [bbotk::ArchiveAsync],
#' i.e., an evaluation has to be among the best across all subspaces to count as a success.
#' Because every worker draws its own sample from the shared posterior,
#' the workers naturally spread over the subspaces instead of all working on the currently most promising one.
#'
#' Counting top-quantile hits instead of comparing objective values directly
#' makes the sampling invariant to the scale of the objective and robust to outliers.
#' The successes are recounted on all finished evaluations in every iteration, so an evaluation that was good early
#' on only stays a success as long as it remains among the best.
#' The prior counts `alpha` and `beta` control how long an algorithm keeps being tried after a streak of failures.
#' Evaluations that raise an error are not counted,
#' so equip the learners with a fallback learner that turns their errors into finished evaluations.
#'
#' A subspace whose parameters are all discrete, i.e., categorical, logical, or bounded integer, has finitely many
#' configurations, e.g., the subspace of a learner without hyperparameters consists of a single configuration.
#' Once all of its configurations are in the archive, the subspace is exhausted and excluded from the sampling.
#' Random points of such a subspace are drawn from the configurations that are not in the archive yet.
#' A worker whose subspaces are all exhausted terminates, and the optimization ends when all workers have
#' terminated, even before the [bbotk::Terminator] signals termination.
#'
#' @section Loop:
#' On each worker, after the shared queue has been emptied:
#'
#' 1. A subspace is selected, the one with the fewest points during the initial design and via Thompson sampling
#'    afterwards.
#' 2. During the initial design, a random point of the subspace is evaluated and the loop starts over.
#' 3. The [SurrogateLearner] is updated on the evaluations and the parameters of the subspace only,
#'    including pending evaluations of the subspace which are imputed.
#' 4. [AcqFunctionStochasticCB] is updated and optimized over the subspace and the resulting point is evaluated.
#'
#' After termination, the surrogate held by the optimizer is updated a final time on *all* evaluations, i.e., across
#' subspaces.
#'
#' @section Surrogate, Acquisition Function, and Acquisition Function Optimizer:
#' As in [OptimizerADBO], the `surrogate`, `acq_function`, and `acq_optimizer` are set by `$optimize()` and
#' components that were set beforehand are overwritten.
#' The [SurrogateLearner] is a random forest, the acquisition function is [AcqFunctionStochasticCB],
#' and the [AcqOptimizer] is a random search with a batch size of 1000 and a budget of 10000 evaluations.
#'
#' [AcqFunctionStochasticCB] samples the \eqn{\lambda} of every worker once from an exponential distribution with rate
#' `1 / lambda`, so that the workers spread over exploration and exploitation.
#' In contrast to [OptimizerADBO], the decay of \eqn{\lambda} is disabled by default, i.e., `rate` is `0`.
#' \eqn{\lambda} belongs to the worker and not to the subspace,
#' i.e., with `rate` greater than `0` the decay counts the iterations of the worker across all the subspaces it samples.
#'
#' @section Parameters:
#' \describe{
#' \item{`branch`}{`character(1)`\cr
#'   Id of the categorical parameter of the search space whose levels define the subspaces.
#'   See section Subspaces.}
#' \item{`subspace_profiles`}{named `character()`\cr
#'   \CRANpkg{mirai} compute profile of each subspace, e.g. `c(tabpfn = "gpu", ranger = "cpu", xgboost = "cpu")`.
#'   Must be named like the levels of the `branch` parameter.
#'   See section Compute Profiles.
#'   Default is `NULL`, i.e., every worker samples among all subspaces.}
#' \item{`top_quantile`}{`numeric(1)`\cr
#'   Fraction of the finished evaluations that count as successes, i.e., an evaluation is a success if its objective
#'   value is among the best `top_quantile` fraction of all finished evaluations.
#'   Default is `0.1`, i.e., the top decile.}
#' \item{`alpha`}{`numeric(1)`\cr
#'   \eqn{\alpha} of the Beta prior, i.e., the prior number of successes of every subspace.
#'   Default is `1`.}
#' \item{`beta`}{`numeric(1)`\cr
#'   \eqn{\beta} of the Beta prior, i.e., the prior number of failures of every subspace.
#'   Default is `1`.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the initial design of every subspace.
#'   See section Initial Design.
#'   Default is `100`.}
#' \item{`initial_design`}{`data.table::data.table()`\cr
#'   Points that are evaluated before the optimization starts.
#'   See section Initial Design.
#'   Default is `NULL`.}
#' \item{`lambda`}{`numeric(1)`\cr
#'   Value used for sampling the lambda of every worker from an exponential distribution.
#'   Default is `1.96`.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay of lambda.
#'   Default is `0`, i.e., the lambda of a worker is not decayed.}
#' \item{`period`}{`integer(1)`\cr
#'   Period of the exponential decay of lambda.
#'   Only used when `rate` is greater than `0`.
#'   Default is `25`.}
#' \item{`catch_errors`}{`logical(1)`\cr
#'   Whether errors of the [SurrogateLearner] and the [AcqOptimizer] are caught.
#'   If `TRUE`, the learner of the surrogate is encapsulated with a fallback learner
#'   and a point that cannot be proposed is replaced by a randomly sampled point.
#'   Set to `FALSE` to raise the errors, e.g., for debugging.
#'   Default is `TRUE`.}
#' \item{`n_workers`}{`integer(1)`\cr
#'   Number of parallel workers.
#'   If `NULL`, all rush workers specified via [rush::rush_plan()] are used.
#'   Default is `NULL`.}
#' \item{`profiles`}{named `integer()`\cr
#'   Number of parallel workers per \CRANpkg{mirai} compute profile, e.g. `c(gpu = 1, cpu = 7)`.
#'   The daemons of every profile must be created with [mirai::daemons()] beforehand.
#'   Cannot be combined with `n_workers`.
#'   If `NULL`, the profiles specified via [rush::rush_plan()] are used.
#'   Default is `NULL`.}
#' }
#'
#' @section Note:
#' A terminator that counts evaluations, e.g. [bbotk::TerminatorEvals], is shared by all subspaces.
#' Subspaces with slow evaluations therefore receive only a small share of the budget.
#' Consider [bbotk::TerminatorRunTime] instead when the subspaces differ strongly in evaluation time.
#'
#' @references
#' * `r format_bib("egele_2023")`
#' * `r format_bib("thompson_1933")`
#' * `r format_bib("thornton_2013")`
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rush") &
#'     requireNamespace("mlr3learners") &
#'     requireNamespace("ranger")) {
#'
#'   if (redis_available()) {
#'
#'     library(bbotk)
#'     library(paradox)
#'     library(mlr3learners)
#'
#'     fun = function(xs) {
#'       list(y = switch(xs$learner, a = xs$a_x^2, b = abs(xs$b_x), c = xs$c_x))
#'     }
#'     domain = ps(
#'       learner = p_fct(c("a", "b", "c")),
#'       a_x = p_dbl(lower = -10, upper = 10, depends = learner == "a"),
#'       b_x = p_dbl(lower = -10, upper = 10, depends = learner == "b"),
#'       c_x = p_dbl(lower = 0, upper = 10, depends = learner == "c")
#'     )
#'     codomain = ps(y = p_dbl(tags = "minimize"))
#'     objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'     instance = OptimInstanceAsyncSingleCrit$new(
#'       objective = objective,
#'       terminator = trm("evals", n_evals = 30))
#'
#'     # learner 'a' runs on the "gpu" profile, learners 'b' and 'c' share the "cpu" profile
#'     mirai::daemons(1, .compute = "gpu")
#'     mirai::daemons(2, .compute = "cpu")
#'     rush::rush_plan(profiles = c(gpu = 1, cpu = 2), worker_type = "mirai")
#'
#'     optimizer = opt("adbo_thompson",
#'       branch = "learner",
#'       subspace_profiles = c(a = "gpu", b = "cpu", c = "cpu"),
#'       design_size = 4)
#'
#'     optimizer$optimize(instance)
#'     mirai::daemons(0, .compute = "gpu")
#'     mirai::daemons(0, .compute = "cpu")
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
OptimizerADBOThompson = R6Class(
  "OptimizerADBOThompson",
  inherit = OptimizerADBO,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        branch = p_uty(custom_check = check_string),
        subspace_profiles = p_uty(custom_check = crate(function(x) {
          check_character(x, any.missing = FALSE, min.chars = 1L, min.len = 1L, names = "unique", null.ok = TRUE)
        })),
        top_quantile = p_dbl(lower = 0, upper = 1, default = 0.1),
        alpha = p_dbl(lower = 0, default = 1),
        beta = p_dbl(lower = 0, default = 1),
        catch_errors = p_lgl(default = TRUE)
      )

      super$initialize(
        id = "adbo_thompson",
        param_set = param_set,
        label = "Asynchronous Decentralized Bayesian Optimization with Thompson Sampling over Subspaces",
        man = "mlr3mbo::mlr_optimizers_adbo_thompson"
      )

      self$param_set$set_values(rate = 0, top_quantile = 0.1, alpha = 1, beta = 1, catch_errors = TRUE)
    },

    #' @description
    #' Performs the optimization on an [bbotk::OptimInstanceAsyncSingleCrit] until termination.
    #' The single evaluations will be written into the [bbotk::ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([bbotk::OptimInstanceAsyncSingleCrit]).
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      pv = self$param_set$values
      search_space = inst$search_space
      branch = pv[["branch"]]

      if (is.null(branch) || branch %nin% search_space$ids() || search_space$class[[branch]] != "ParamFct") {
        error_config("'branch' must be the id of a categorical parameter of the search space.")
      }

      subspace_profiles = pv[["subspace_profiles"]]
      if (!is.null(subspace_profiles)) {
        assert_names(
          names(subspace_profiles),
          permutation.of = search_space$levels[[branch]],
          .var.name = "names of 'subspace_profiles'"
        )
        # the workers started with `n_workers` and the worker of the debug mode run on no compute profile
        profiles = if (is.null(pv[["n_workers"]]) && !getOption("bbotk.debug", FALSE)) {
          pv[["profiles"]] %??% rush::rush_config()$profiles
        }
        if (!is.null(profiles)) {
          assert_set_equal(names(profiles), unique(subspace_profiles), .var.name = "names of 'profiles'")
        }
      }

      super$optimize(inst)
    }
  ),

  private = list(
    # the initial design of a subspace is proposed by its workers, only a user-supplied design is queued
    .initial_design = function(inst) {
      if (!inst$archive$n_evals) self$param_set$values[["initial_design"]]
    },

    .optimize = function(inst) {
      lg = lgr::get_logger("mlr3/bbotk")
      pv = self$param_set$values
      archive = inst$archive
      branch = pv[["branch"]]
      col_y = archive$cols_y
      cols_x = archive$cols_x
      na_x = na_values(inst$search_space)
      # the bandit counts top-quantile hits, so the objective values are always oriented towards minimization
      y_mult = mult_max_to_min(archive$codomain)[[col_y]]

      # a worker samples among the subspaces of the compute profile it runs on
      subspaces = partition_search_space(inst$search_space, param = branch)
      profile = inst$rush$profile
      subspace_profiles = pv[["subspace_profiles"]]
      arms = if (is.null(profile) || is.null(subspace_profiles)) {
        names(subspaces)
      } else {
        names(subspace_profiles)[subspace_profiles == profile]
      }
      if (!length(arms)) {
        error_config("No subspace is assigned to compute profile '%s'. Check 'subspace_profiles'.", profile)
      }
      lg$info("Worker '%s' samples among subspace(s) %s", inst$rush$worker_id, str_collapse(arms, quote = "'"))
      # a subspace with finitely many configurations can be exhausted
      n_configurations = map_dbl(subspaces[arms], function(subspace) nrow(subspace_grid(subspace)) %??% Inf)

      self$surrogate$param_set$set_values(catch_errors = pv[["catch_errors"]])
      self$acq_optimizer$param_set$set_values(catch_errors = pv[["catch_errors"]])
      if (!pv[["catch_errors"]]) {
        self$surrogate$learner$encapsulate(method = "none")
      }

      get_private(inst)$.eval_queue()

      current = NULL
      while (!inst$is_terminated) {
        # the subspace of a point is its level of the branching parameter, which is known in every state
        data = archive$data
        # a single symbol as `i` is evaluated in the calling scope and cannot clash with a column of the archive
        is_finished = which(data[["state"]] == "finished")
        finished = data[is_finished]
        arm_data = set_names(map(arms, function(arm) {
          in_arm = which(data[[branch]] == arm)
          data[in_arm]
        }), arms)

        open = discard(arms, function(arm) {
          subspace_exhausted(subspaces[[arm]], arm_data[[arm]], n_configurations[[arm]])
        })
        if (!length(open)) {
          lg$info("All subspaces of worker '%s' are exhausted, the worker terminates", inst$rush$worker_id)
          break
        }

        # the initial design is proposed on the fly, always for a subspace with the fewest points;
        # ties are broken at random, so that workers that start at the same time spread over the subspaces
        n = map_int(arm_data[open], nrow)
        arm = if (any(n < pv[["design_size"]])) {
          open[[shuffle(which(n == min(n)), 1L)]]
        } else {
          thompson_sample_subspace(
            open,
            subspace = finished[[branch]],
            y = finished[[col_y]] * y_mult,
            top_quantile = pv[["top_quantile"]],
            alpha = pv[["alpha"]],
            beta = pv[["beta"]]
          )
        }
        subspace = subspaces[[arm]]

        # the surrogate cannot be trained without any finished evaluation of the subspace
        xdt = if (n[[arm]] >= pv[["design_size"]] && any(finished[[branch]] == arm)) {
          if (!identical(arm, current)) {
            private$.restrict_to_subspace(subspace, branch)
            current = arm
          }
          tryCatch(
            {
              self$acq_function$surrogate$update()
              self$acq_function$update()
              self$acq_optimizer$optimize()
            },
            Mlr3ErrorMbo = function(cond) {
              lg$warn("Caught the following error: %s", cond$message)
              lg$info("Proposing a randomly sampled point")
              NULL
            }
          )
        }
        # a subspace with finitely many configurations only proposes configurations that are not in the archive yet
        xdt = xdt %??% generate_point_subspace(subspace, arm_data[[arm]])

        get_private(inst)$.eval_point(pad_xs(transpose_list(xdt)[[1L]], cols_x = cols_x, na_values = na_x))
      }
    },

    # restrict the surrogate, the acquisition function and the acquisition function optimizer of this worker to the
    # evaluations and the parameters of `subspace`
    .restrict_to_subspace = function(subspace, branch) {
      level = subspace$levels[[branch]]
      self$surrogate$cols_x = subspace$ids()
      self$surrogate$row_filter = crate(function(data) data[[branch]] %in% level, branch, level)

      # assigning the surrogate resets the domain of the acquisition function to the full support of `cols_x`,
      # so the restricted domain has to be set afterwards
      self$acq_function$surrogate = self$surrogate
      self$acq_function$domain = strip_trafo(subspace)
      self$acq_optimizer$acq_function = self$acq_function
    }
  )
)

#' @include aaa.R
optimizers[["adbo_thompson"]] = OptimizerADBOThompson
