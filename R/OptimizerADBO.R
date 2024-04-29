#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_optimizers_adbo
#'
#' @description
#' Asynchronous Decentralized Bayesian Optimization (ADBO).
#'
#' @note
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
  inherit = OptimizerAsync,

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
    #' Performs the optimization on a [OptimInstanceAsyncSingleCrit] or [OptimInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([OptimInstanceAsyncSingleCrit] | [OptimInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      pv = self$param_set$values

      # initial design
      design = if (is.null(pv$initial_design)) {

        lg$debug("Generating sobol design with size %s", pv$design_size)
        generate_design_sobol(inst$search_space, n = pv$design_size)$data
      } else {

        lg$debug("Using provided initial design with size %s", nrow(pv$initial_design))
        pv$initial_design
      }

      optimize_async_default(inst, self, design)
    }
  ),

  private = list(

    .optimize = function(inst) {
      pv = self$param_set$values
      search_space = inst$search_space
      archive = inst$archive

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
      evaluate_queue_default(inst)

      lg$debug("Optimizer '%s' starts the tuning phase", self$id)

      # actual loop
      while (!inst$is_terminated) {

        # decrease lambda
        if (pv$exponential_decay) {
          lambda = lambda_0 * exp(-pv$rate * (t %% pv$period))
          t = t + 1
        } else {
          lambda = pv$lambda
        }

        # sample
        acq_function$constants$set_values(lambda = lambda)
        acq_function$surrogate$update()
        acq_function$update()
        xdt = acq_optimizer$optimize()

        # transpose point
        xss = transpose_list(xdt)
        xs = xss[[1]][inst$archive$cols_x]
        lg$trace("Optimizer '%s' draws %s", self$id, as_short_string(xs))
        xs_trafoed = trafo_xs(xs, search_space)

        # eval
        key = archive$push_running_point(xs)
        ys = inst$objective$eval(xs_trafoed)

        # push result
        extra = c(xss[[1]][c("acq_cb", ".already_evaluated")], list(lambda_0 = lambda_0, lambda = lambda))
        archive$push_result(key, ys, x_domain = xs_trafoed, extra = extra)
      }
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
