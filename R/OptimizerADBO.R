#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_optimizers_adbo
#'
#' @description
#' Asynchronous Decentralized Bayesian Optimization (ADBO).
#'
#' @notes
#' The \eqn{\lambda} parameter of the upper confidence bound acquisition function controls the trade-off between exploration and exploitation.
#' A large \eqn{\lambda} value leads to more exploration, while a small \eqn{\lambda} value leads to more exploitation.
#' ADBO can use periodic exponential decay to reduce \eqn{\lambda} periodically to the exploitation phase.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`\cr
#'   \eqn{\lambda} value used for the confidence bound.
#'   Defaults to `1.96`.}
#' \item{`exponential_decay`}{`lgl(1)`\cr
#'   Whether to use periodic exponential decay for \eqn{\lambda}.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`t`}{`integer(1)`\cr
#'   Period of the exponential decay.}
#' \item{`initial_design_size`}{`integer(1)`\cr
#'   Size of the initial design.}
#' \item{`initial_design`}{`data.table`\cr
#'   Initial design.}
#' }
#'
#' @export
OptimizerADBO = R6Class("OptimizerADBO",
  inherit = bbotk::Optimizer,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        lambda = p_dbl(lower = 0, default = 1.96),
        exponential_decay = p_lgl(default = TRUE),
        rate = p_dbl(lower = 0, default = 0.1),
        period = p_int(lower = 1L, default = 25L),
        design_size = p_int(lower = 1L),
        initial_design = p_uty(),
        impute_method = p_fct(c("mean", "random"), default = "random")
      )

      param_set$set_values(lambda = 1.96, exponential_decay = TRUE, rate = 0.1, period = 25L, design_size = 1L, impute_method = "random")

      super$initialize("adbo",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit"),
        packages = "mlr3mbo",
        label = "Asynchronous Decentralized Bayesian Optimization",
        man = "mlr3mbo::OptimizerADBO")
    },


    #' @description
    #' Performs the optimization and writes optimization result into
    #' [OptimInstance]. The optimization result is returned but the complete
    #' optimization path is stored in [Archive] of [OptimInstance].
    #'
    #' @param inst ([OptimInstance]).
    #' @return [data.table::data.table].
    optimize = function(inst) {

      # generate initial design
      pv = self$param_set$values
      design = if (is.null(pv$initial_design)) {
        generate_design_sobol(inst$search_space, n = pv$design_size)$data
      } else {
        pv$initial_design
      }

      # send initial design to workers
      inst$rush$push_tasks(transpose_list(design), extra = list(list(timestamp_xs = Sys.time())))

      # optimize
      inst$archive$start_time = Sys.time()
      result = optimize_decentralized(inst, self, private)

      # FIXME: kill workers to increase the chance of a fitting the final model
      inst$rush$stop_workers(type = "kill")

      result
    }
  ),

  private = list(

    .optimize = function(inst) {
      pv = self$param_set$values
      search_space = inst$search_space
      rush = inst$rush

      # sample lambda from exponential distribution
      lambda_0 = rexp(1, 1 / pv$lambda)
      t = 0

      surrogate = default_surrogate(inst)
      surrogate$param_set$set_values(impute_missings = pv$impute_method)
      acq_function = acqf("cb", lambda = runif(1, 1 , 3))
      acq_optimizer = acqo(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 10000L))

      surrogate$archive = inst$archive
      acq_function$surrogate = surrogate
      acq_optimizer$acq_function = acq_function

      lg$debug("Optimizer '%s' evaluates the initial design", self$id)

      # evaluate initial design
      while (rush$n_queued_tasks > 0) {
        task = rush$pop_task(fields = "xs")
        xs_trafoed = trafo_xs(task$xs, inst$search_space)
        ys = inst$objective$eval(xs_trafoed)
        rush$push_results(task$key, yss = list(ys), extra = list(list(x_domain = list(xs_trafoed), timestamp_ys = Sys.time(), stage = "initial_design")))
      }

      lg$debug("Optimizer '%s' starts the tuning phase", self$id)

      # actual loop
      while (!inst$is_terminated) {

        if (pv$exponential_decay) {
          lambda = lambda_0 * exp(-pv$rate * (t %% pv$period))
          t = t + 1
        } else {
          lambda = pv$lambda
        }

        acq_function$constants$set_values(lambda = lambda)
        acq_function$surrogate$update()
        acq_function$update()
        xdt = acq_optimizer$optimize()
        xss = transpose_list(xdt)
        xs = xss[[1]][inst$archive$cols_x]
        lg$trace("Optimizer '%s' draws %s", self$id, as_short_string(xs))
        xs_trafoed = trafo_xs(xs, search_space)
        extra = xss[[1]][c("acq_cb", ".already_evaluated")]
        keys = rush$push_running_task(list(xs), extra = list(list(timestamp_xs = Sys.time())))
        ys = inst$objective$eval(xs_trafoed)
        rush$push_results(keys, yss = list(ys), extra = list(c(extra, list(
          x_domain = list(xs_trafoed),
          timestamp_ys = Sys.time(),
          stage = "mbo",
          lambda_0 = lambda_0,
          lambda = lambda))))
      }
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
