#' @title Asynchronous Decentralized Bayesian Optimization on Homogeneous Subspaces
#'
#' @include OptimizerAsyncMbo.R
#' @name mlr_optimizers_adbo_subspaces
#'
#' @description
#' `OptimizerADBOSubspaces` runs Asynchronous Decentralized Bayesian Optimization (ADBO) on a partition of the search
#' space into homogeneous subspaces.
#' Every worker is permanently assigned to exactly one subspace and only ever proposes and evaluates points of that
#' subspace.
#' Because the workers already run independently of each other in ADBO, each of them can fit its own
#' [SurrogateLearner] on the evaluations of its own subspace only.
#'
#' This is useful in two situations that usually occur together.
#' First, when the search space is strongly hierarchical, e.g., an AutoML search space in which a branching parameter
#' selects the learner, a surrogate fitted on the full search space has to model a highly conditional response
#' surface with many inactive (`NA`) parameters.
#' Restricting the surrogate to a homogeneous subspace removes most of these dependencies and yields a much
#' lower-dimensional and much better behaved modeling problem.
#' Second, when different parts of the search space require different hardware, e.g., learners that can only be
#' trained on a GPU versus learners that are trained on CPU cores.
#' Running every subspace on its own \CRANpkg{mirai} compute profile then directly controls how much of each resource
#' is used.
#'
#' `OptimizerADBOSubspaces` is considered an experimental feature and the API might be subject to changes.
#' Currently, only single-objective optimization is supported.
#'
#' @section Subspaces:
#' The subspaces are passed as a named list of [paradox::ParamSet]s via the `subspaces` parameter.
#' Every subspace must be a subset of the search space of the [bbotk::OptimInstanceAsyncSingleCrit] and the subspaces
#' must be disjoint, i.e., every evaluated point must belong to exactly one subspace.
#' Use [partition_search_space()] to derive such a list from a branching parameter of the search space.
#'
#' Each evaluation is logged into the [bbotk::ArchiveAsync] with an additional `.subspace` column holding the id of
#' the subspace it was proposed from.
#' The points of the initial designs already carry `.subspace` while they are queued.
#' For the points proposed by the workers, `.subspace` is written when the evaluation finishes, as for all other
#' columns of a finished evaluation, and is therefore `NA` for the running and failed points that the
#' [bbotk::ArchiveAsync] also contains.
#'
#' @section Compute Profiles:
#' The workers are divided among the subspaces by the
#' [compute profiles](https://mirai.r-lib.org/articles/mirai.html#scoped-profiles) of \CRANpkg{mirai}.
#' Every subspace runs on its own profile and a worker derives its subspace from the profile it runs on.
#' By default, a subspace runs on the profile of the same name.
#' Use `subspace_profiles` to map the subspaces to differently named profiles.
#'
#' The daemons of every profile must be created beforehand and the number of workers per subspace is set with the
#' `profiles` parameter of [OptimizerAsyncMbo] or with [rush::rush_plan()], e.g., one worker on the single GPU and
#' seven workers on the CPU cores of a machine.
#'
#' ```
#' mirai::daemons(1, .compute = "gpu")
#' mirai::daemons(7, .compute = "cpu")
#'
#' opt("adbo_subspaces",
#'   subspaces = subspaces,
#'   subspace_profiles = c(nn = "gpu", rf = "cpu"),
#'   profiles = c(gpu = 1L, cpu = 7L))
#' ```
#'
#' Every compute profile also has its own queue in \CRANpkg{rush} which is used to hand the initial design of a
#' subspace to its workers, see section Initial Design.
#' Points that are pushed to the shared queue instead, e.g. by a callback, are popped by any worker and therefore have
#' no `.subspace`.
#'
#' Deriving the subspace from the compute profile keeps the assignment stable when \CRANpkg{rush} restarts a worker
#' that died.
#' It also leaves the setup of a worker to the daemons of its profile, e.g. binding a worker to a device with
#' `mirai::daemons(env = list(CUDA_VISIBLE_DEVICES = "0"), .compute = "gpu")`.
#' The `n_workers` parameter of [OptimizerAsyncMbo] must not be used, since all its workers would run on the default
#' compute profile which is not assigned to any subspace.
#'
#' @section Initial Design:
#' Every subspace receives its own initial design of `design_size` points, generated with `design_function` on the
#' subspace.
#' Use `design_size_subspace` to set a different size per subspace, which is usually necessary when the subspaces
#' differ strongly in dimensionality or in how many evaluations they can afford.
#' Alternatively, `initial_design_subspace` accepts a named list of [data.table::data.table()]s.
#'
#' In contrast to [OptimizerAsyncMbo], the initial design is not pushed to the shared queue of \CRANpkg{rush},
#' because any worker could pop any point from it and would thereby leave its subspace.
#' Instead, the design of a subspace is pushed to the queue of the compute profile of that subspace, so that only the
#' workers of the subspace pop its points.
#' The `initial_design` and `design_size` parameters of [OptimizerAsyncMbo] therefore behave slightly differently and
#' `initial_design` must not be set.
#'
#' @section Loop:
#' On each worker, after the queue of its compute profile has been emptied:
#'
#' 1. The [SurrogateLearner] is updated on the evaluations of the subspace only, including pending evaluations of the
#'    subspace which are imputed.
#' 2. [AcqFunctionStochasticCB] is updated, sampling and decaying its own \eqn{\lambda} exactly as in
#'    [OptimizerADBO].
#' 3. The acquisition function is optimized over the subspace and the resulting point is evaluated.
#'
#' Unless a surrogate, acquisition function, or acquisition function optimizer has been set on the optimizer,
#' the [SurrogateLearner] defaults to a random forest, the acquisition function to [AcqFunctionStochasticCB], and the
#' [AcqOptimizer] to a random search with a batch size of 1000 and a budget of 10000 evaluations.
#' After termination, the surrogate held by the optimizer is updated a final time on *all* evaluations, i.e., across
#' subspaces.
#'
#' @section Parameters:
#' \describe{
#' \item{`subspaces`}{named `list()` of [paradox::ParamSet]\cr
#'   Partition of the search space.
#'   See section Subspaces.}
#' \item{`subspace_profiles`}{named `character()`\cr
#'   \CRANpkg{mirai} compute profile of each subspace, e.g. `c(nn = "gpu", rf = "cpu")`.
#'   Must be named like `subspaces` and every subspace must run on its own profile.
#'   See section Compute Profiles.
#'   Default is `NULL`, i.e., every subspace runs on the profile of the same name.}
#' \item{`initial_design_subspace`}{named `list()` of [data.table::data.table()]\cr
#'   Initial design per subspace.
#'   If `NULL`, a design is generated for every subspace with the specified `design_function`.
#'   Default is `NULL`.}
#' \item{`design_size_subspace`}{named `integer()`\cr
#'   Size of the generated initial design per subspace.
#'   Overrides `design_size` for the named subspaces.
#'   Default is `NULL`, i.e., `design_size` is used for every subspace.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the generated initial design of every subspace.
#'   Default is `100`.}
#' \item{`design_function`}{`character(1)`\cr
#'   Sampling function to generate the initial designs.
#'   Can be `random` [paradox::generate_design_random], `lhs` [paradox::generate_design_lhs],
#'   or `sobol` [paradox::generate_design_sobol].
#'   Default is `sobol`.}
#' \item{`lambda`}{`numeric(1)`\cr
#'   Value used for sampling the lambda for each worker from an exponential distribution.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`period`}{`integer(1)`\cr
#'   Period of the exponential decay.}
#' }
#'
#' @section Note:
#' A terminator that counts evaluations, e.g. [bbotk::TerminatorEvals], is shared by all subspaces.
#' Subspaces with slow evaluations therefore receive only a small share of the budget.
#' Consider [bbotk::TerminatorRunTime] instead when the subspaces differ strongly in evaluation time.
#'
#' @references
#' * `r format_bib("egele_2023")`
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
#'       list(y = if (xs$branch == "a") xs$a_x^2 else abs(xs$b_x))
#'     }
#'     domain = ps(
#'       branch = p_fct(c("a", "b")),
#'       a_x = p_dbl(lower = -10, upper = 10, depends = branch == "a"),
#'       b_x = p_dbl(lower = -10, upper = 10, depends = branch == "b")
#'     )
#'     codomain = ps(y = p_dbl(tags = "minimize"))
#'     objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'     instance = OptimInstanceAsyncSingleCrit$new(
#'       objective = objective,
#'       terminator = trm("evals", n_evals = 20))
#'
#'     subspaces = partition_search_space(
#'       instance$search_space,
#'       param = "branch",
#'       groups = list(a = "a", b = "b"))
#'
#'     # one worker per subspace, each on its own compute profile
#'     mirai::daemons(1, .compute = "a")
#'     mirai::daemons(1, .compute = "b")
#'     rush::rush_plan(profiles = c(a = 1, b = 1), worker_type = "mirai")
#'
#'     optimizer = opt("adbo_subspaces",
#'       subspaces = subspaces,
#'       design_size = 4)
#'
#'     optimizer$optimize(instance)
#'     mirai::daemons(0, .compute = "a")
#'     mirai::daemons(0, .compute = "b")
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
OptimizerADBOSubspaces = R6Class(
  "OptimizerADBOSubspaces",
  inherit = OptimizerAsyncMbo,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        subspaces = p_uty(custom_check = crate(function(x) {
          check_list(x, types = "ParamSet", min.len = 1L, names = "unique", any.missing = FALSE)
        })),
        subspace_profiles = p_uty(custom_check = crate(function(x) {
          check_character(x, any.missing = FALSE, min.chars = 1L, min.len = 1L, names = "unique", null.ok = TRUE)
        })),
        initial_design_subspace = p_uty(custom_check = crate(function(x) {
          check_list(x, types = "data.table", names = "unique", null.ok = TRUE)
        })),
        design_size_subspace = p_uty(custom_check = crate(function(x) {
          check_integerish(x, lower = 1L, min.len = 1L, any.missing = FALSE, names = "unique", null.ok = TRUE)
        })),
        lambda = p_dbl(lower = 0, default = 1.96),
        rate = p_dbl(lower = 0, default = 0.1),
        period = p_int(lower = 1L, default = 25L)
      )

      super$initialize(
        id = "adbo_subspaces",
        param_set = param_set,
        label = "Asynchronous Decentralized Bayesian Optimization on Homogeneous Subspaces",
        man = "mlr3mbo::mlr_optimizers_adbo_subspaces"
      )

      self$param_set$set_values(lambda = 1.96, rate = 0.1, period = 25L)
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
      subspaces = assert_subspaces(pv[["subspaces"]], inst = inst)
      subspace_ids = names(subspaces)

      if (!is.null(pv[["initial_design"]])) {
        error_config(
          paste(
            "'%s' generates one initial design per subspace.",
            "Set 'initial_design_subspace' instead of 'initial_design'."
          ),
          self$id
        )
      }

      subspace_profiles = private$.assert_subspace_profiles(pv[["subspace_profiles"]], subspace_ids)
      # a worker looks up its subspace by the compute profile it runs on
      private$.subspace_of_profile = set_names(subspace_ids, subspace_profiles[subspace_ids])

      # the acquisition function is optimized on the untransformed subspace, mirroring `generate_acq_domain()`
      private$.acq_domains = map(subspaces, strip_trafo)

      if (is.null(self$acq_function)) {
        self$acq_function = AcqFunctionStochasticCB$new(
          distribution = "exponential",
          lambda = pv[["lambda"]],
          rate = pv[["rate"]],
          period = pv[["period"]]
        )
      }

      if (is.null(self$surrogate)) {
        self$surrogate = self$acq_function$surrogate %??% default_surrogate(inst, force_random_forest = TRUE)
      }

      if (is.null(self$acq_optimizer)) {
        self$acq_optimizer = AcqOptimizer$new(
          optimizer = opt("random_search", batch_size = 1000L),
          terminator = trm("evals", n_evals = 10000L)
        )
      }

      if (is.null(self$result_assigner)) {
        self$result_assigner = default_result_assigner(inst)
      }

      self$surrogate$reset()
      self$acq_function$reset()
      self$acq_optimizer$reset()

      self$surrogate$archive = inst$archive
      self$acq_function$surrogate = self$surrogate
      self$acq_optimizer$acq_function = self$acq_function

      check_packages_installed(
        self$packages,
        msg = sprintf("Package '%%s' required but not installed for Optimizer '%s'", format(self))
      )

      if (!is.null(pv[["n_workers"]])) {
        error_config(
          paste(
            "'%s' divides the workers among the subspaces by the mirai compute profiles.",
            "Set 'profiles' instead of 'n_workers'."
          ),
          self$id
        )
      }

      profiles = private$.assert_profiles(pv[["profiles"]], subspace_profiles)

      lg = lgr::get_logger("mlr3/bbotk")
      lg$info(
        "Optimizing %i subspace(s): %s",
        length(subspaces),
        str_collapse(sprintf("'%s' on compute profile '%s'", subspace_ids, subspace_profiles[subspace_ids]))
      )

      # every subspace hands its initial design to its own workers through the queue of its compute profile
      private$.push_designs(inst, subspaces, subspace_profiles)

      result = optimize_async_default(inst, self, design = NULL, profiles = profiles)

      # the workers only update copies of the surrogate that are restricted to their subspace,
      # so the final update on all data must happen on the main process
      tryCatch(
        {
          self$surrogate$update()
        },
        error = function(error_condition) {
          lg$warn("Could not update the surrogate a final time after the optimization process has terminated.")
        }
      )

      result
    }
  ),

  private = list(
    # subspace id per compute profile, e.g. `c(gpu = "nn", cpu = "rf")`;
    # a worker looks up the subspace of the profile it runs on when it starts
    .subspace_of_profile = NULL,

    # search space of the acquisition function optimization per subspace
    .acq_domains = NULL,

    # push the initial design of every subspace to the queue of its compute profile, so that only the workers of
    # that subspace pop its points
    .push_designs = function(inst, subspaces, subspace_profiles) {
      designs = private$.generate_designs(subspaces)
      cols_x = inst$archive$cols_x
      na_x = na_values(inst$search_space)
      # the debug mode runs a single worker in the main process which has no compute profile and therefore only
      # pops from the shared queue
      debug = getOption("bbotk.debug", FALSE)

      iwalk(designs, function(design, subspace_id) {
        xss = map(transpose_list(design), function(xs) pad_xs(xs, cols_x = cols_x, na_values = na_x))
        inst$archive$push_points(
          xss,
          xss_extra = list(list(.subspace = subspace_id)),
          profile = if (!debug) subspace_profiles[[subspace_id]]
        )
      })

      invisible(designs)
    },

    .generate_designs = function(subspaces) {
      pv = self$param_set$values
      subspace_ids = names(subspaces)

      if (!is.null(pv[["initial_design_subspace"]])) {
        assert_permutation(
          names(pv[["initial_design_subspace"]]),
          subspace_ids,
          .var.name = "names of 'initial_design_subspace'"
        )
        return(map(pv[["initial_design_subspace"]][subspace_ids], function(design) as.data.table(design)))
      }

      design_size = set_names(rep(pv[["design_size"]], length(subspaces)), subspace_ids)
      if (!is.null(pv[["design_size_subspace"]])) {
        assert_subset(names(pv[["design_size_subspace"]]), subspace_ids, .var.name = "names of 'design_size_subspace'")
        design_size[names(pv[["design_size_subspace"]])] = as.integer(pv[["design_size_subspace"]])
      }

      generate_design = switch(
        pv[["design_function"]],
        "random" = generate_design_random,
        "sobol" = generate_design_sobol,
        "lhs" = generate_design_lhs
      )

      set_names(map(subspace_ids, function(subspace_id) {
        generate_design(subspaces[[subspace_id]], n = design_size[[subspace_id]])$data
      }), subspace_ids)
    },

    # every subspace runs on its own compute profile, otherwise a worker cannot derive its subspace from the
    # profile it runs on
    .assert_subspace_profiles = function(subspace_profiles, subspace_ids) {
      subspace_profiles = subspace_profiles %??% set_names(subspace_ids, subspace_ids)
      assert_permutation(names(subspace_profiles), subspace_ids, .var.name = "names of 'subspace_profiles'")

      duplicated_profiles = unique(subspace_profiles[duplicated(subspace_profiles)])
      if (length(duplicated_profiles)) {
        error_config(
          "Compute profile(s) %s are assigned to more than one subspace. Every subspace must run on its own profile.",
          str_collapse(duplicated_profiles, quote = "'")
        )
      }

      subspace_profiles
    },

    # the workers are divided among the subspaces by the compute profiles, so every profile of a subspace must
    # receive workers
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

      assert_permutation(names(profiles), unname(subspace_profiles), .var.name = "names of 'profiles'")
      profiles
    },

    # restrict the surrogate, the acquisition function and the acquisition function optimizer of this worker
    # to `subspace_id`
    .restrict_to_subspace = function(inst, subspace_id) {
      subspace = self$param_set$values[["subspaces"]][[subspace_id]]

      self$surrogate$cols_x = subspace$ids()
      self$surrogate$row_filter = crate(function(data) subspace_contains(subspace, data), subspace)

      # assigning the surrogate resets the domain of the acquisition function to the full support of `cols_x`,
      # so the restricted domain has to be set afterwards
      self$acq_function$surrogate = self$surrogate
      self$acq_function$domain = private$.acq_domains[[subspace_id]]
      self$acq_optimizer$acq_function = self$acq_function
    },

    .optimize = function(inst) {
      lg = lgr::get_logger("mlr3/bbotk")

      # a worker takes the subspace of the compute profile it runs on
      profile = inst$rush$profile
      subspace_id = if (is.null(profile) && getOption("bbotk.debug", FALSE)) {
        # the debug mode runs a single worker in the main process, which has no compute profile
        private$.subspace_of_profile[[1L]]
      } else {
        # `[[` on a missing name errors, `[` marks the missing profile with `NA`
        unname(private$.subspace_of_profile[profile %??% ""])
      }

      if (is.na(subspace_id)) {
        error_config(
          "No subspace is assigned to compute profile '%s'. Check 'subspace_profiles' and the daemons of the profile.",
          profile %??% "default"
        )
      }
      lg$info("Worker '%s' is assigned to subspace '%s'", inst$rush$worker_id, subspace_id)

      private$.restrict_to_subspace(inst, subspace_id)

      cols_x = inst$archive$cols_x
      na_x = na_values(inst$search_space)

      lg$debug("Optimizer '%s' evaluates the initial design of subspace '%s'", self$id, subspace_id)
      get_private(inst)$.eval_queue()

      lg$debug("Optimizer '%s' starts the optimization phase on subspace '%s'", self$id, subspace_id)
      while (!inst$is_terminated) {
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
    },

    .propose_random = function(inst, subspace_id, cols_x, na_values) {
      xdt = generate_design_random(self$param_set$values[["subspaces"]][[subspace_id]], n = 1L)$data
      pad_xs(transpose_list(xdt)[[1L]], cols_x = cols_x, na_values = na_values)
    }
  )
)

# subspaces must partition the search space of the instance, otherwise evaluations cannot be attributed to exactly
# one surrogate
assert_subspaces = function(subspaces, inst) {
  assert_list(subspaces, types = "ParamSet", min.len = 1L, names = "unique", any.missing = FALSE)
  search_space = inst$search_space

  walk(names(subspaces), function(subspace_id) {
    ids = subspaces[[subspace_id]]$ids()
    if (!test_subset(ids, search_space$ids())) {
      error_config(
        "Subspace '%s' contains parameter(s) %s that are not part of the search space.",
        subspace_id,
        str_collapse(setdiff(ids, search_space$ids()), quote = "'")
      )
    }
  })

  # every point of every subspace must be rejected by all other subspaces
  walk(names(subspaces), function(subspace_id) {
    design = generate_design_random(subspaces[[subspace_id]], n = 100L)$data
    overlapping = keep(setdiff(names(subspaces), subspace_id), function(other) {
      any(subspace_contains(subspaces[[other]], design))
    })
    if (length(overlapping)) {
      error_config(
        "Subspace '%s' overlaps with subspace(s) %s. Subspaces must be disjoint.",
        subspace_id,
        str_collapse(overlapping, quote = "'")
      )
    }
  })

  subspaces
}

# drop the trafos of the individual parameters, mirroring `generate_acq_domain()`
strip_trafo = function(search_space) {
  domains = map(search_space$domains, function(domain) {
    domain$.trafo[1] = list(NULL)
    domain
  })
  do.call(ps, domains)
}

# typed `NA` per parameter, used to mark parameters outside of a subspace as inactive
na_values = function(search_space) {
  map(search_space$class, function(class) {
    switch(class, ParamDbl = NA_real_, ParamInt = NA_integer_, ParamFct = NA_character_, NA)
  })
}

# a point of a subspace only holds the parameters of that subspace, but the archive expects all parameters of the
# search space; the remaining ones are inactive and therefore `NA`
pad_xs = function(xs, cols_x, na_values) {
  missing = setdiff(cols_x, names(xs))
  c(xs, na_values[missing])
}

#' @include aaa.R
optimizers[["adbo_subspaces"]] = OptimizerADBOSubspaces
