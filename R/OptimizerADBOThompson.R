#' @title Asynchronous Decentralized Bayesian Optimization with Thompson Sampling over Subspaces
#'
#' @include OptimizerADBOSubspaces.R
#' @name mlr_optimizers_adbo_thompson
#'
#' @description
#' `OptimizerADBOThompson` runs Asynchronous Decentralized Bayesian Optimization (ADBO) on a search space that is
#' partitioned into homogeneous subspaces and samples the subspace to work on via Thompson sampling.
#' The typical use case is combined algorithm selection and hyperparameter optimization with one subspace per
#' algorithm, e.g., derived with [partition_search_space()] along the branching parameter of an
#' [mlr3pipelines::Graph] that selects the learner.
#'
#' The optimizer extends [OptimizerADBOSubspaces] by a second level of grouping.
#' In [OptimizerADBOSubspaces], every subspace runs on its own \CRANpkg{mirai} compute profile and a worker is
#' permanently assigned to exactly one subspace.
#' In `OptimizerADBOThompson`, several subspaces may share a compute profile, e.g., all learners that are trained on CPU
#' cores run on the `"cpu"` profile while the learners that require a GPU run on the `"gpu"` profile.
#' A worker is still permanently assigned to its compute profile, but in each iteration it samples one of the
#' subspaces of its profile via Thompson sampling and proposes a point of that subspace only.
#' The hardware is therefore a property of *where* a subspace runs, whereas the algorithm is a property of *what*
#' the subspace is.
#' With exactly one subspace per compute profile, `OptimizerADBOThompson` behaves like [OptimizerADBOSubspaces].
#'
#' `OptimizerADBOThompson` is considered an experimental feature and the API might be subject to changes.
#' Currently, only single-objective optimization is supported.
#'
#' @section Subspaces and Compute Profiles:
#' The subspaces are passed as a named list of [paradox::ParamSet]s via the `subspaces` parameter and must partition
#' the search space, see the section Subspaces of [OptimizerADBOSubspaces].
#' Use [partition_search_space()] with one group per algorithm to derive them from the parameter that selects the
#' algorithm.
#'
#' `subspace_profiles` maps every subspace to the compute profile it runs on and, in contrast to
#' [OptimizerADBOSubspaces], several subspaces may map to the same profile.
#' By default, every subspace runs on the profile of the same name, which reduces the optimizer to
#' [OptimizerADBOSubspaces].
#'
#' ```
#' mirai::daemons(1, .compute = "gpu")
#' mirai::daemons(7, .compute = "cpu")
#'
#' opt("adbo_thompson",
#'   subspaces = subspaces,
#'   subspace_profiles = c(tabpfn = "gpu", ranger = "cpu", xgboost = "cpu"),
#'   profiles = c(gpu = 1L, cpu = 7L))
#' ```
#'
#' Every subspace receives its own initial design, which is pushed to the queue of its compute profile as in
#' [OptimizerADBOSubspaces].
#' The queue of a profile that is shared by several subspaces therefore holds the designs of all of them and any
#' worker of the profile pops any of these points.
#'
#' @section Thompson Sampling:
#' Each subspace is an arm of a Beta-Bernoulli bandit.
#' An evaluation counts as a success if its objective value is among the best `top_quantile` fraction of all finished
#' evaluations, and as a failure otherwise.
#' The single best evaluation is always a success and evaluations that share the objective value at the boundary are
#' all successes.
#' With \eqn{s_k} successes and \eqn{f_k} failures of subspace \eqn{k}, a value is sampled from the posterior
#' \eqn{\theta_k \sim \mathrm{Beta}(\alpha + s_k, \beta + f_k)} and the subspace with the largest \eqn{\theta_k} is
#' selected.
#'
#' A worker only samples among the subspaces of its own compute profile, but the successes and failures are counted
#' on all finished evaluations of the [bbotk::ArchiveAsync], i.e., an evaluation has to be among the best across all
#' profiles to count as a success.
#' Because every worker draws its own sample from the shared posterior, the workers of a profile naturally spread
#' over its subspaces instead of all working on the currently most promising one.
#'
#' Counting top-quantile hits instead of comparing objective values directly makes the sampling invariant to the
#' scale of the objective and robust to outliers.
#' The successes are recounted on all finished evaluations in every iteration, so an evaluation that was good early
#' on only stays a success as long as it remains among the best.
#' The prior counts `alpha` and `beta` control how long an algorithm keeps being tried after a streak of failures.
#'
#' A subspace whose parameters are all discrete, i.e., categorical, logical, or bounded integer, has finitely many
#' configurations, e.g., the subspace of a learner without hyperparameters consists of a single configuration.
#' Once all of its configurations have been evaluated, the subspace is exhausted and excluded from the sampling, so
#' that no configuration is evaluated twice.
#' The generated initial design of such a subspace is capped at the number of its configurations for the same
#' reason.
#' A worker whose subspaces are all exhausted terminates, and the optimization ends when all workers have
#' terminated, even before the [bbotk::Terminator] signals termination.
#'
#' @section Loop:
#' On each worker, after the queue of its compute profile has been emptied:
#'
#' 1. One of the subspaces of the compute profile of the worker is sampled via Thompson sampling.
#' 2. If the sampled subspace differs from the one of the previous iteration, the [SurrogateLearner], the acquisition
#'    function, and the [AcqOptimizer] are restricted to the sampled subspace.
#' 3. The [SurrogateLearner] is updated on the evaluations of the sampled subspace only, including pending evaluations
#'    of the subspace which are imputed.
#' 4. [AcqFunctionStochasticCB] is updated, sampling and decaying its own \eqn{\lambda} exactly as in
#'    [OptimizerADBO].
#'    The decay continues across the subspaces a worker samples.
#' 5. The acquisition function is optimized over the sampled subspace and the resulting point is evaluated.
#'
#' The defaults for the [SurrogateLearner], the acquisition function, and the [AcqOptimizer] are those of
#' [OptimizerADBOSubspaces].
#'
#' @section Parameters:
#' \describe{
#' \item{`subspaces`}{named `list()` of [paradox::ParamSet]\cr
#'   Partition of the search space, usually one subspace per algorithm.
#'   See section Subspaces and Compute Profiles.}
#' \item{`subspace_profiles`}{named `character()`\cr
#'   \CRANpkg{mirai} compute profile of each subspace, e.g. `c(tabpfn = "gpu", ranger = "cpu", xgboost = "cpu")`.
#'   Must be named like `subspaces`.
#'   Several subspaces may run on the same profile.
#'   See section Subspaces and Compute Profiles.
#'   Default is `NULL`, i.e., every subspace runs on the profile of the same name.}
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
#' }
#'
#' The remaining parameters `profiles`, `n_workers`, `initial_design_subspace`, `initial_design`,
#' `design_size_subspace`, `design_size`, `design_function`, `lambda`, `rate`, and `period` are those of
#' [OptimizerADBOSubspaces].
#'
#' @inheritSection mlr_optimizers_adbo_subspaces Initial Design
#'
#' @section Note:
#' The surrogate of a worker is fitted on the evaluations of the sampled subspace only, but an acquisition function
#' that requires an incumbent, e.g. [mlr_acqfunctions_ei], reads the incumbent from the whole
#' [bbotk::ArchiveAsync].
#' If the surrogate uses an [OutputTrafo] that is not inverted for the posterior, the transformation is fitted on the
#' sampled subspace and cannot represent the outcomes of the other subspaces.
#' Use the default [AcqFunctionStochasticCB], which does not require an incumbent, or a surrogate without an
#' [OutputTrafo] in this case.
#'
#' A terminator that counts evaluations, e.g. [bbotk::TerminatorEvals], is shared by all subspaces.
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
#'     # one subspace per learner
#'     subspaces = partition_search_space(instance$search_space, param = "learner")
#'
#'     # learner 'a' runs on the "gpu" profile, learners 'b' and 'c' share the "cpu" profile
#'     mirai::daemons(1, .compute = "gpu")
#'     mirai::daemons(2, .compute = "cpu")
#'     rush::rush_plan(profiles = c(gpu = 1, cpu = 2), worker_type = "mirai")
#'
#'     optimizer = opt("adbo_thompson",
#'       subspaces = subspaces,
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
  inherit = OptimizerADBOSubspaces,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        top_quantile = p_dbl(lower = 0, upper = 1, default = 0.1),
        alpha = p_dbl(lower = 0, default = 1),
        beta = p_dbl(lower = 0, default = 1)
      )

      super$initialize(
        id = "adbo_thompson",
        param_set = param_set,
        label = "Asynchronous Decentralized Bayesian Optimization with Thompson Sampling over Subspaces",
        man = "mlr3mbo::mlr_optimizers_adbo_thompson"
      )

      self$param_set$set_values(top_quantile = 0.1, alpha = 1, beta = 1)
    }
  ),

  private = list(
    # the generated design of a subspace with finitely many configurations is capped at that number
    .generate_designs = function(subspaces) {
      designs = super$.generate_designs(subspaces)
      if (!is.null(self$param_set$values[["initial_design_subspace"]])) {
        return(designs)
      }
      imap(designs, function(design, subspace_id) {
        grid = subspace_grid(subspaces[[subspace_id]])
        if (is.null(grid)) design else grid[sample.int(nrow(grid), min(nrow(design), nrow(grid)))]
      })
    },

    # several subspaces may share a compute profile, so only the names have to match the subspaces
    .assert_subspace_profiles = function(subspace_profiles, subspace_ids) {
      subspace_profiles = subspace_profiles %??% set_names(subspace_ids, subspace_ids)
      assert_permutation(names(subspace_profiles), subspace_ids, .var.name = "names of 'subspace_profiles'")
      subspace_profiles
    },

    # every profile that is assigned to at least one subspace must receive workers
    .assert_profiles = function(profiles, subspace_profiles) {
      if (getOption("bbotk.debug", FALSE)) {
        # the debug mode runs a single worker in the main process which has no compute profile
        return(NULL)
      }

      profiles = profiles %??% rush::rush_config()$profiles
      if (is.null(profiles)) {
        error_config(
          paste(
            "'%s' divides the workers among the subspaces by the mirai compute profiles.",
            "Set the 'profiles' parameter or `rush::rush_plan(profiles = ...)` instead of 'n_workers'."
          ),
          self$id
        )
      }

      assert_permutation(names(profiles), unique(unname(subspace_profiles)), .var.name = "names of 'profiles'")
      profiles
    },

    .optimize = function(inst) {
      lg = lgr::get_logger("mlr3/bbotk")
      pv = self$param_set$values

      # `.subspace_of_profile` holds one entry per subspace named by its profile, so a profile that is shared by
      # several subspaces appears several times
      subspaces_of_profile = split(unname(private$.subspace_of_profile), names(private$.subspace_of_profile))

      # a worker samples among the subspaces of the compute profile it runs on
      profile = inst$rush$profile
      subspace_ids = if (is.null(profile) && getOption("bbotk.debug", FALSE)) {
        # the debug mode runs a single worker in the main process, which has no compute profile
        unlist(subspaces_of_profile, use.names = FALSE)
      } else {
        subspaces_of_profile[[profile %??% ""]]
      }

      if (is.null(subspace_ids)) {
        error_config(
          "No subspace is assigned to compute profile '%s'. Check 'subspace_profiles' and the daemons of the profile.",
          profile %??% "default"
        )
      }
      lg$info(
        "Worker '%s' is assigned to subspace(s) %s",
        inst$rush$worker_id,
        str_collapse(subspace_ids, quote = "'")
      )

      cols_x = inst$archive$cols_x
      col_y = inst$archive$cols_y
      na_x = na_values(inst$search_space)
      subspaces = pv[["subspaces"]][subspace_ids]
      # a subspace with finitely many configurations can be exhausted, so its number of configurations is needed
      n_configurations = map_dbl(subspaces, function(subspace) nrow(subspace_grid(subspace)) %??% Inf)
      # the bandit counts top-quantile hits, so the objective values are always oriented towards minimization
      y_mult = mult_max_to_min(inst$archive$codomain)[[col_y]]

      lg$debug("Optimizer '%s' evaluates the initial designs of subspace(s) %s", self$id, str_collapse(subspace_ids))
      get_private(inst)$.eval_queue()

      lg$debug("Optimizer '%s' starts the optimization phase", self$id)
      current = NULL
      while (!inst$is_terminated) {
        finished = inst$archive$finished_data
        # points that were pushed to the shared queue, e.g. by a callback, carry no `.subspace` and count for no arm
        finished_subspace = finished[[".subspace"]] %??% rep(NA_character_, nrow(finished))

        # exhausted subspaces have nothing left to evaluate and are excluded from the sampling
        exhausted = map_lgl(subspace_ids, function(subspace_id) {
          subspace_exhausted(
            subspaces[[subspace_id]],
            finished[which(finished_subspace == subspace_id)],
            n_configurations[[subspace_id]]
          )
        })
        if (all(exhausted)) {
          lg$info("All subspaces of worker '%s' are exhausted, the worker terminates", inst$rush$worker_id)
          break
        }

        subspace_id = thompson_sample_subspace(
          subspace_ids[!exhausted],
          subspace = finished_subspace,
          y = (finished[[col_y]] %??% numeric()) * y_mult,
          top_quantile = pv[["top_quantile"]],
          alpha = pv[["alpha"]],
          beta = pv[["beta"]]
        )

        if (!identical(subspace_id, current)) {
          lg$debug("Thompson sampling selected subspace '%s'", subspace_id)
          private$.restrict_to_subspace(inst, subspace_id)
          current = subspace_id
        }

        xs = if (inst$archive$n_finished == 0L) {
          # the surrogate cannot be trained without any finished evaluation
          # this happens when a worker reaches this point before the initial design has been evaluated
          lg$info("No finished evaluations available yet. Proposing a randomly sampled point")
          private$.propose_random(inst, subspace_id, cols_x, na_x)
        } else {
          tryCatch(
            {
              self$acq_function$surrogate$update()
              self$acq_function$update()
              xdt = self$acq_optimizer$optimize()
              pad_xs(transpose_list(xdt)[[1L]], cols_x = cols_x, na_values = na_x)
            },
            Mlr3ErrorMbo = function(cond) {
              lg$warn("Caught the following error: %s", cond$message)
              lg$info("Proposing a randomly sampled point")
              private$.propose_random(inst, subspace_id, cols_x, na_x)
            }
          )
        }

        xs[[".subspace"]] = subspace_id
        get_private(inst)$.eval_point(xs)
      }
    }
  )
)

#' @include aaa.R
optimizers[["adbo_thompson"]] = OptimizerADBOThompson
