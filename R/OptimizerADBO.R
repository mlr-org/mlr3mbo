#' @export
OptimizerADBO = R6Class("OptimizerADBO",
  inherit = bbotk::Optimizer,

  public = list(

    initialize = function() {
      param_set = ps(
        init_design_size = p_int(lower = 1L),
        initial_design = p_uty()
      )

      super$initialize("adbo",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "multi-crit", "single-crit"),
        packages = "mlr3mbo",
        label = "Asynchronous Decentralized Bayesian Optimization",
        man = "mlr3mbo::OptimizerADBO")
    },

    optimize = function(inst) {

      if (rush_available()) {
        inst$archive$start_time = Sys.time()
        inst$.__enclos_env__$private$.context = ContextOptimization$new(instance = inst, optimizer = self)
        call_back("on_optimization_begin", inst$callbacks, get_private(inst)$.context)

        # generate initial design
        pv = self$param_set$values
        design = if (is.null(pv$initial_design)) {
          generate_design_sobol(inst$search_space, n = pv$init_design_size)$data
        } else {
          pv$initial_design
        }

        # send initial design to workers
        inst$rush$push_tasks(transpose_list(design), extra = list(list(timestamp_xs = Sys.time())))

        # start rush workers
        inst$rush$start_workers(
          worker_loop = bbotk_worker_loop_decentralized,
          packages = c("bbotk", "mlr3mbo"),
          optimizer = self,
          instance = inst,
          lgr_thresholds = c(bbotk = "debug", rush = "debug"),
          wait_for_workers = TRUE)
      } else {
        stop("No rush plan available. See `?rush::rush_plan()`")
      }

      lg$info("Starting to optimize %i parameter(s) with '%s' and '%s' on %i worker(s)",
        inst$search_space$length,
        self$format(),
        inst$terminator$format(with_params = TRUE),
        inst$rush$n_running_workers
      )

      # wait
      while(!inst$is_terminated) {
        Sys.sleep(1)
        inst$rush$print_log()
        inst$rush$detect_lost_workers()
      }

      inst$rush$stop_workers(type = "kill")

      # assign result
      private$.assign_result(inst)

      # assign result
      private$.assign_result(inst)
      lg$info("Finished optimizing after %i evaluation(s)", inst$archive$n_evals)
      lg$info("Result:")
      lg$info(capture.output(print(inst$result, lass = FALSE, row.names = FALSE, print.keys = FALSE)))
      call_back("on_optimization_end", inst$callbacks, get_private(inst)$.context)
      return(inst$result)
    }
  ),

  private = list(

    .optimize_remote = function(inst) {
      search_space = inst$search_space
      rush = inst$rush

      surrogate = default_surrogate(inst)
      surrogate$param_set$set_values(impute_missings = TRUE)
      acq_function = acqf("cb", lambda = runif(1, 1 , 3))
      acq_optimizer = acqo(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 10000L))

      surrogate$archive = inst$archive
      acq_function$surrogate = surrogate
      acq_optimizer$acq_function = acq_function

      # evaluate initial design
      while (rush$n_queued_tasks > 0) {
        lg$debug("Evaluating initial design")
        task = rush$pop_task(fields = "xs")
        xs_trafoed = trafo_xs(task$xs, inst$search_space)
        ys = inst$objective$eval(xs_trafoed)
        rush$push_results(task$key, yss = list(ys), extra = list(list(x_domain = list(xs_trafoed), timestamp_ys = Sys.time(), stage = "initial_design")))
      }

      # actual loop
      while (!inst$is_terminated) {
        lg$debug("Optimizing")
        acq_function$surrogate$update()
        acq_function$update()
        xdt = acq_optimizer$optimize()
        xss = transpose_list(xdt)
        xs = xss[[1]][inst$archive$cols_x]
        xs_trafoed = trafo_xs(xs, search_space)
        extra = xss[[1]][c("acq_cb", ".already_evaluated")]

        keys = rush$push_running_task(list(xs), extra = list(list(timestamp_xs = Sys.time())))
        ys = inst$objective$eval(xs_trafoed)
        rush$push_results(keys, yss = list(ys), extra = list(c(extra, list(x_domain = list(xs_trafoed), timestamp_ys = Sys.time(), stage = "mbo"))))
      }
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
