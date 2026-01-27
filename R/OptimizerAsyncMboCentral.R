#' @title Centralized Asynchronous Model Based Optimization
#'
#' @name mlr_optimizers_async_mbo_central
#'
#' @description
#' `OptimizerAsyncMboCentral` class that implements Centralized Asynchronous Model Based Optimization.
#' In contrast to [OptimizerAsyncMbo], the main process is responsible for fitting the surrogate model and optimizing the acquisition function.
#' Workers only evaluate points asynchronously.
#' This design ensures that new points are generated as soon as workers become idle, maximizing resource utilization.
#'
#' The optimizer follows a modular layout in which the surrogate model, acquisition function, and acquisition optimizer can be customized.
#' The [SurrogateLearner] will impute missing values due to pending evaluations.
#' A stochastic [AcqFunction], e.g., [AcqFunctionStochasticEI] or [AcqFunctionStochasticCB] can be used to promote exploration.
#'
#' Currently, only single-objective optimization is supported and `OptimizerAsyncMboCentral` is considered an experimental feature and API might be subject to changes.
#'
#' Note that in general the [SurrogateLearner] is updated one final time on all available data after the optimization process has terminated.
#' However, in certain scenarios this is not always possible or meaningful.
#' It is therefore recommended to manually inspect the [SurrogateLearner] after optimization if it is to be used, e.g., for visualization purposes to make
#' sure that it has been properly updated on all available data.
#' If this final update of the [SurrogateLearner] could not be performed successfully, a warning will be logged.
#'
#' By specifying a [ResultAssigner], one can alter how the final result is determined after optimization, e.g.,
#' simply based on the evaluations logged in the archive [ResultAssignerArchive] or based on the [Surrogate] via [ResultAssignerSurrogate].
#'
#' @section Archive:
#' The [bbotk::ArchiveAsync] holds the following additional columns that are specific to MBO algorithms:
#'   * `acq_function$id` (`numeric(1)`)\cr
#'     The value of the acquisition function.
#'   * `".already_evaluated"` (`logical(1))`\cr
#'     Whether this point was already evaluated. Depends on the `skip_already_evaluated` parameter of the [AcqOptimizer].
#'
#' If the [bbotk::ArchiveAsync] does not contain any evaluations prior to optimization, an initial design is needed.
#' If the `initial_design` parameter is specified to be a `data.table`, this data will be used.
#' Otherwise, if it is `NULL`, an initial design of size `design_size` will be generated based on the `generate_design` sampling function.
#' See also the parameters below.
#'
#' @section Parameters:
#' \describe{
#' \item{`initial_design`}{`data.table::data.table()`\cr
#'   Initial design of the optimization.
#'   If `NULL`, a design of size `design_size` is generated with the specified `design_function`.
#'   Default is `NULL`.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the initial design if it is to be generated.
#'   Default is `100`.}
#' \item{`design_function`}{`character(1)`\cr
#'   Sampling function to generate the initial design.
#'   Can be `random` [paradox::generate_design_random], `lhs` [paradox::generate_design_lhs], or `sobol` [paradox::generate_design_sobol].
#'   Default is `sobol`.}
#' \item{`n_workers`}{`integer(1)`\cr
#'   Number of parallel workers.
#'   If `NULL`, all rush workers specified via [rush::rush_plan()] are used.
#'   Default is `NULL`.}
#' }
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rush") &
#'     requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'
#'   if (redis_available()) {
#'
#'     library(bbotk)
#'     library(paradox)
#'     library(mlr3learners)
#'
#'     fun = function(xs) {
#'       list(y = xs$x ^ 2)
#'     }
#'     domain = ps(x = p_dbl(lower = -10, upper = 10))
#'     codomain = ps(y = p_dbl(tags = "minimize"))
#'     objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'     instance = OptimInstanceAsyncSingleCrit$new(
#'       objective = objective,
#'       terminator = trm("evals", n_evals = 10))
#'
#'     mirai::daemons(2)
#'     rush::rush_plan(n_workers = 2, worker_type = "remote")
#'
#'     optimizer = opt("async_mbo_central", design_size = 4, n_workers = 2)
#'
#'     optimizer$optimize(instance)
#'     mirai::daemons(0)
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
OptimizerAsyncMboCentral = R6Class("OptimizerAsyncMboCentral",
  inherit = bbotk::OptimizerAsync,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' If `surrogate` is `NULL` and the `acq_function$surrogate` field is populated, this [SurrogateLearner] is used.
    #' Otherwise, `default_surrogate(instance)` is used.
    #' If `acq_function` is `NULL` and the `acq_optimizer$acq_function` field is populated, this [AcqFunction] is used (and therefore its `$surrogate` if populated; see above).
    #' Otherwise `default_acqfunction(instance)` is used.
    #' If `acq_optimizer` is `NULL`, `default_acqoptimizer(instance)` is used.
    #'
    #' Even if already initialized, the `surrogate$archive` field will always be overwritten by the [bbotk::ArchiveAsync] of the current [bbotk::OptimInstanceAsyncSingleCrit] to be optimized.
    #'
    #' For more information on default values for `surrogate`, `acq_function`, `acq_optimizer` and `result_assigner`, see `?mbo_defaults`.
    #'
    #' @template param_id
    #' @template param_surrogate
    #' @template param_acq_function
    #' @template param_acq_optimizer
    #' @template param_result_assigner
    #' @template param_label
    #' @param param_set ([paradox::ParamSet])\cr
    #'  Set of control parameters.
    #' @template param_man
    initialize = function(
      id = "async_mbo_central",
      surrogate = NULL,
      acq_function = NULL,
      acq_optimizer = NULL,
      result_assigner = NULL,
      param_set = NULL,
      label = "Centralized Asynchronous Model Based Optimization",
      man = "mlr3mbo::OptimizerAsyncMboCentral"
      ) {

      default_param_set = ps(
        initial_design = p_uty(),
        design_size = p_int(lower = 1, default = 100L),
        design_function = p_fct(c("random", "sobol", "lhs"), default = "sobol"),
        n_workers = p_int(lower = 1L)
      )
      param_set = c(default_param_set, param_set)

      param_set$set_values(design_size = 100L, design_function = "sobol")

      super$initialize("async_mbo_central",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),  # is replaced with dynamic AB after construction
        properties = c("dependencies", "single-crit"),  # is replaced with dynamic AB after construction
        packages = c("mlr3mbo", "rush"),  # is replaced with dynamic AB after construction
        label = label,
        man = man)

      self$surrogate = assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      self$acq_function = assert_r6(acq_function, classes = "AcqFunction", null.ok = TRUE)
      self$acq_optimizer = assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
      self$result_assigner = assert_r6(result_assigner, classes = "ResultAssigner", null.ok = TRUE)
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), if (is.na(self$label)) "" else paste0(": ", self$label))
      catn(str_indent("* Parameter classes:", self$param_classes))
      catn(str_indent("* Properties:", self$properties))
      catn(str_indent("* Packages:", self$packages))
      catn(str_indent("* Surrogate:", if (is.null(self$surrogate)) "-" else self$surrogate$print_id))
      catn(str_indent("* Acquisition Function:", if (is.null(self$acq_function)) "-" else class(self$acq_function)[1L]))
      catn(str_indent("* Acquisition Function Optimizer:", if (is.null(self$acq_optimizer)) "-" else self$acq_optimizer$print_id))
      catn(str_indent("* Result Assigner:", if (is.null(self$result_assigner)) "-" else class(self$result_assigner)[1L]))
    },

    #' @description
    #' Reset the optimizer.
    #' Sets the following fields to `NULL`:
    #' `surrogate`, `acq_function`, `acq_optimizer`,`result_assigner`
    #' Resets parameter values `design_size` and `design_function` to their defaults.
    reset = function() {
      private$.surrogate = NULL
      private$.acq_function = NULL
      private$.acq_optimizer = NULL
      private$.result_assigner = NULL
      self$param_set$set_values(design_size = 100L, design_function = "sobol")
    },

    #' @description
    #' Performs the optimization on an [bbotk::OptimInstanceAsyncSingleCrit] until termination.
    #' The main process fits the surrogate model and optimizes the acquisition function.
    #' Workers only evaluate points from the queue.
    #' The single evaluations will be written into the [bbotk::ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([bbotk::OptimInstanceAsyncSingleCrit]).
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      # setup MBO components
      if (is.null(self$acq_function)) {
        self$acq_function = self$acq_optimizer$acq_function %??% default_acqfunction(inst)
      }

      if (is.null(self$surrogate)) {  # acq_function$surrogate has precedence
        self$surrogate = self$acq_function$surrogate %??% default_surrogate(inst)
      }

      if (is.null(self$acq_optimizer)) {
        self$acq_optimizer = default_acqoptimizer(self$acq_function, inst)
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

      check_packages_installed(self$packages, msg = sprintf("Package '%%s' required but not installed for Optimizer '%s'", format(self)))

      pv = self$param_set$values
      n_workers = pv$n_workers

      # initial design
      design = if (inst$archive$n_evals) {
        lg$debug("Using archive with %s evaluations as initial design", inst$archive$n_evals)
        NULL
      } else if (is.null(pv$initial_design)) {
        # generate initial design
        generate_design = switch(pv$design_function,
          "random" = generate_design_random,
          "sobol" = generate_design_sobol,
          "lhs" = generate_design_lhs)

        lg$debug("Generating sobol design with size %s", pv$design_size)
        generate_design(inst$search_space, n = pv$design_size)$data
      } else {
        # use provided initial design
        lg$debug("Using provided initial design with size %s", nrow(pv$initial_design))
        pv$initial_design
      }

      # initialize optimization
      inst$archive$start_time = Sys.time()
      get_private(inst)$.initialize_context(self)
      call_back("on_optimization_begin", inst$objective$callbacks, inst$objective$context)

      # send initial design to queue
      if (!is.null(design)) {
        inst$archive$push_points(transpose_list(design))
      }

      rush = inst$rush
      worker_type = rush::rush_config()$worker_type %??% "local"

      if (getOption("bbotk.debug", FALSE)) {
        # debug mode runs .optimize() in main process
        rush = rush::RushWorker$new(inst$rush$network_id, remote = FALSE)
        inst$rush = rush
        inst$archive$rush = rush
        worker_type = "debug_local"

        call_back("on_worker_begin", inst$objective$callbacks, inst$objective$context)

        # run optimizer loop - in debug mode, just evaluate the queue
        get_private(self)$.optimize(inst)

        call_back("on_worker_end", inst$objective$callbacks, inst$objective$context)
      } else {
        # start workers that only evaluate the queue
        if (worker_type == "script") {
          rush$worker_script(
            worker_loop = bbotk::bbotk_worker_loop,
            packages = c(self$packages, inst$objective$packages, "bbotk"),
            optimizer = self,
            instance = inst)

          rush$wait_for_workers(n = 1)
        } else if (worker_type == "remote") {
          worker_ids = rush$start_remote_workers(
            n_workers = n_workers,
            worker_loop = bbotk::bbotk_worker_loop,
            packages = c(self$packages, inst$objective$packages, "bbotk"),
            optimizer = self,
            instance = inst)

          rush$wait_for_workers(n = 1, worker_ids)
        } else if (worker_type == "local") {
          worker_ids = rush$start_local_workers(
            n_workers = n_workers,
            worker_loop = bbotk::bbotk_worker_loop,
            packages = c(self$packages, inst$objective$packages, "bbotk"),
            optimizer = self,
            instance = inst)

          rush$wait_for_workers(n = 1, worker_ids)
        }
      }

      lg$info("Starting to optimize %i parameter(s) with '%s' and '%s' on %s %s worker(s)",
        inst$search_space$length,
        self$format(),
        inst$terminator$format(with_params = TRUE),
        as.character(rush::rush_config()$n_workers %??% ""),
        worker_type)

      n_running_workers = 0

      # main loop: wait for evaluations, update surrogate, propose new points
      while (!inst$is_terminated) {
        Sys.sleep(0.1)

        if (rush$n_running_workers > n_running_workers) {
          n_running_workers = rush$n_running_workers
          lg$info("%i worker(s) running", n_running_workers)
        }

        # print logger messages from workers
        rush$print_log()

        # print evaluations
        if (getOption("bbotk.tiny_logging", FALSE)) {
          bbotk::tiny_logging(inst, self)
        } else {
          new_results = inst$rush$fetch_new_tasks()
          if (nrow(new_results)) {
            lg$info("Results of %i configuration(s):", nrow(new_results))
            setcolorder(new_results, c(inst$archive$cols_y, inst$archive$cols_x, "timestamp_xs", "timestamp_ys"))
            cns = setdiff(colnames(new_results), c("pid", "x_domain", "keys"))
            lg$info(capture.output(print(new_results[, cns, with = FALSE], class = FALSE, row.names = FALSE, print.keys = FALSE)))
          }
        }

        rush$detect_lost_workers()

        if (!rush$n_running_workers) {
          lg$info("All workers have terminated.")
          break
        }

        # check queue status and propose new points if needed
        # we need at least one finished evaluation to fit the surrogate
        n_finished = inst$archive$n_finished

        if (n_finished >= 1 && !inst$is_terminated) {
          # check if there are idle workers (not enough points in queue/running)
          # we want to keep the queue filled so workers don't idle
          n_queued = inst$archive$n_queued
          n_running = inst$archive$n_running

          # propose new points if queue is running low
          # aim to have at least n_workers points queued/running
          n_to_propose = max(0, rush$n_running_workers - n_queued - n_running)

          if (n_to_propose > 0) {
            for (i in seq_len(n_to_propose)) {
              if (inst$is_terminated) break

              # propose a new point using MBO
              # update surrogate on each iteration to account for newly queued points
              xdt = tryCatch({
                start_time = Sys.time()
                self$acq_function$surrogate$update()
                time_surrogate = Sys.time() - start_time
                self$acq_function$update()
                start_time = Sys.time()
                xdt = self$acq_optimizer$optimize()
                time_acq_optimizer = Sys.time() - start_time
                xdt
              }, mbo_error = function(mbo_error_condition) {
                lg$info(paste0(class(mbo_error_condition), collapse = " / "))
                lg$info("Proposing a randomly sampled point")
                xdt = generate_design_random(inst$search_space, n = 1L)$data
                time_surrogate = 0
                time_acq_optimizer = 0
                xdt
              })
              # push the new point to the queue
              xs = transpose_list(xdt)[[1]][inst$archive$cols_x]
              inst$archive$push_points(list(xs), extra = list(list(time_surrogate = time_surrogate, time_acq_optimizer = time_acq_optimizer)))
              lg$debug("Proposed new point and pushed to queue")
            }
          }
        }
      }

      # move queued and running tasks to failed
      failed_tasks = unlist(rush$tasks_with_state(states = c("queued", "running")))
      if (length(failed_tasks)) {
        rush$push_failed(failed_tasks, conditions = replicate(length(failed_tasks), list(message = "Optimization terminated"), simplify = FALSE))
      }

      if (!inst$archive$n_finished) {
        stopf("Optimization terminated without any finished evaluations.")
      }

      # final surrogate update
      tryCatch(
        {
          self$surrogate$update()
        }, surrogate_update_error = function(error_condition) {
          lg$warn("Could not update the surrogate a final time after the optimization process has terminated.")
        }
      )

      # assign result
      get_private(self)$.assign_result(inst)
      lg$info("Finished optimizing after %i evaluation(s)", inst$rush$n_finished_tasks)
      lg$info("Result:")

      # print result
      if (getOption("bbotk.tiny_logging", FALSE)) {
        bbotk::tiny_result(inst, self)
      } else {
        lg$info(capture.output(print(inst$result, class = FALSE, row.names = FALSE, print.keys = FALSE)))
      }

      call_back("on_optimization_end", inst$objective$callbacks, inst$objective$context)
      inst$rush$stop_workers(type = "kill")
      return(inst$result)
    }
  ),

  active = list(
    #' @template field_surrogate
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        private$.surrogate = assert_r6(rhs, classes = "SurrogateLearner", null.ok = TRUE)
      }
    },

    #' @template field_acq_function
    acq_function = function(rhs) {
      if (missing(rhs)) {
        private$.acq_function
      } else {
        private$.acq_function = assert_r6(rhs, classes = "AcqFunction", null.ok = TRUE)
      }
    },

    #' @template field_acq_optimizer
    acq_optimizer = function(rhs) {
      if (missing(rhs)) {
        private$.acq_optimizer
      } else {
        private$.acq_optimizer = assert_r6(rhs, classes = "AcqOptimizer", null.ok = TRUE)
      }
    },

    #' @template field_result_assigner
    result_assigner = function(rhs) {
      if (missing(rhs)) {
        private$.result_assigner
      } else {
        private$.result_assigner = assert_r6(rhs, classes = "ResultAssigner", null.ok = TRUE)
      }
    },

    #' @template field_param_classes
    param_classes = function(rhs) {
      if (missing(rhs)) {
        param_classes_surrogate = c("logical" = "ParamLgl", "integer" = "ParamInt", "numeric" = "ParamDbl", "factor" = "ParamFct")
        if (!is.null(self$surrogate)) {
          param_classes_surrogate = param_classes_surrogate[c("logical", "integer", "numeric", "factor") %in% self$surrogate$feature_types] # surrogate has precedence over acq_function$surrogate
        }
        param_classes_acq_opt = if (!is.null(self$acq_optimizer)) {
          self$acq_optimizer$optimizer$param_classes
        } else {
          c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct")
        }
        unname(intersect(param_classes_surrogate, param_classes_acq_opt))
      } else {
        stop("$param_classes is read-only.")
      }
    },

    #' @template field_properties
    properties = function(rhs) {
      if (missing(rhs)) {
        properties_loop_function = "single-crit"
        properties_surrogate = "dependencies"
        if (!is.null(self$surrogate)) {
          if ("missings" %nin% self$surrogate$properties) {
            properties_surrogate = character()
          }
        }
        unname(c(properties_surrogate, properties_loop_function))
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_packages
    packages = function(rhs) {
      if (missing(rhs)) {
        union(c("mlr3mbo", "rush"), c(self$acq_function$packages, self$surrogate$packages, self$acq_optimizer$optimizer$packages, self$result_assigner$packages))
      } else {
        stop("$packages is read-only.")
      }
    }
  ),

  private = list(
    .surrogate = NULL,
    .acq_function = NULL,
    .acq_optimizer = NULL,
    .result_assigner = NULL,

    # Worker loop: continuously pop and evaluate points from the queue until terminated
    .optimize = function(inst) {
      while (!inst$is_terminated) {
        # try to pop a point from the queue
        task = inst$archive$pop_point()

        if (!is.null(task)) {
          # evaluate the point
          xs_trafoed = bbotk:::trafo_xs(task$xs, inst$search_space)

          call_back("on_optimizer_queue_before_eval", inst$objective$callbacks, inst$objective$context)

          ys = inst$objective$eval(xs_trafoed)

          call_back("on_optimizer_queue_after_eval", inst$objective$callbacks, inst$objective$context)

          inst$archive$push_result(task$key, ys, x_domain = xs_trafoed)
        } else {
          # no points in queue, wait for main process to add more
          Sys.sleep(0.1)
        }
      }
    },

    .assign_result = function(inst) {
      self$result_assigner$assign_result(inst)
    }
  )
)

#' @include aaa.R
optimizers[["async_mbo_central"]] = OptimizerAsyncMboCentral
