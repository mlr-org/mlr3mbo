#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_optimizers_adbo
#'
#' @description
#' `OptimizerADBO` class that implements Asynchronous Decentralized Bayesian Optimization (ADBO).
#' ADBO is a variant of Asynchronous Model Based Optimization (AMBO) that uses
#' [AcqFunctionStochasticCB] with exponential lambda decay.
#'
#' Currently, only single-objective optimization is supported and
#' [OptimizerADBO] is considered an experimental feature and API might be subject to changes.
#'
#' @note
#' The lambda parameter of the confidence bound acquisition function controls the trade-off between
#' exploration and exploitation.
#' A large lambda value leads to more exploration, while a small lambda value leads to more exploitation.
#' The initial lambda value of the acquisition function used on each worker is drawn from an
#' exponential distribution with rate `1 / lambda`.
#' ADBO can use periodic exponential decay to reduce lambda periodically for a given time step `t` with the formula
#' `lambda * exp(-rate * (t %% period))`.
#' The [SurrogateLearner] is configured to use a random forest and
#' the [AcqOptimizer] is a random search with a batch size of 1000 and a budget of 10000 evaluations.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`\cr
#'   Value used for sampling the lambda for each worker from an exponential distribution.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`period`}{`integer(1)`\cr
#'   Period of the exponential decay.}
#' }
#' @template params_async_mbo
#'
#' @references
#' * `r format_bib("egele_2023")`
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
#'     rush::rush_plan(n_workers=2, worker_type = "mirai")
#'
#'     optimizer = opt("adbo", design_size = 4, n_workers = 2)
#'
#'     optimizer$optimize(instance)
#'     mirai::daemons(0)
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
OptimizerADBO = R6Class(
  "OptimizerADBO",
  inherit = OptimizerAsyncMbo,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        lambda = p_dbl(lower = 0, default = 1.96),
        rate = p_dbl(lower = 0, default = 0.1),
        period = p_int(lower = 1L, default = 25L)
      )

      super$initialize(
        id = "adbo",
        param_set = param_set,
        label = "Asynchronous Decentralized Bayesian Optimization",
        man = "mlr3mbo::OptimizerADBO"
      )

      self$param_set$set_values(
        lambda = 1.96,
        rate = 0.1,
        period = 25L
      )
    },

    #' @description
    #' Performs the optimization on an [bbotk::OptimInstanceAsyncSingleCrit] until termination.
    #' The single evaluations will be written into the [bbotk::ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([bbotk::OptimInstanceAsyncSingleCrit]).
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      self$acq_function = AcqFunctionStochasticCB$new(
        distribution = "exponential",
        lambda = self$param_set$values$lambda,
        rate = self$param_set$values$rate,
        period = self$param_set$values$period
      )

      self$surrogate = default_surrogate(inst, force_random_forest = TRUE)

      self$acq_optimizer = AcqOptimizer$new(
        optimizer = opt("random_search", batch_size = 1000L),
        terminator = trm("evals", n_evals = 10000L)
      )

      super$optimize(inst)
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
