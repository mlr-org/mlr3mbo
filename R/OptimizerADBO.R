#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_optimizers_adbo
#'
#' @description
#' `OptimizerADBO` class that implements Asynchronous Decentralized Bayesian Optimization (ADBO).
#' ADBO is a variant of Asynchronous Model Based Optimization (AMBO) that uses [AcqFunctionStochasticCB] with exponential lambda decay.
#'
#' Currently, only single-objective optimization is supported and [OptimizerADBO] is considered an experimental feature and API might be subject to changes.
#'
#' @note
#' The lambda parameter of the confidence bound acquisition function controls the trade-off between exploration and exploitation.
#' A large lambda value leads to more exploration, while a small lambda value leads to more exploitation.
#' The initial lambda value of the acquisition function used on each worker is drawn from an exponential distribution with rate `1 / lambda`.
#' ADBO can use periodic exponential decay to reduce lambda periodically for a given time step `t` with the formula `lambda * exp(-rate * (t %% period))`.
#' The [SurrogateLearner] is configured to use a random forest and the [AcqOptimizer] is a random search with a batch size of 1000 and a budget of 10000 evaluations.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`\cr
#'   Value used for sampling the lambda for each worker from an exponential distribution.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`period`}{`integer(1)`\cr
#'   Period of the exponential decay.}
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
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceAsyncSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 10))
#'
#'   rush::rush_plan(n_workers=2)
#'
#'   optimizer = opt("adbo", design_size = 4, n_workers = 2)
#'
#'   optimizer$optimize(instance)
#' }
#' }
OptimizerADBO = R6Class("OptimizerADBO",
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
        man = "mlr3mbo::OptimizerADBO")

      self$param_set$set_values(
        lambda = 1.96,
        rate = 0.1,
        period = 25L)
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
        lambda = self$param_set$values$lambda,
        rate = self$param_set$values$rate,
        period = self$param_set$values$period
      )

      self$surrogate = default_surrogate(inst, force_random_forest = TRUE)

      self$acq_optimizer = AcqOptimizer$new(
        optimizer = opt("random_search", batch_size = 1000L),
        terminator = trm("evals", n_evals = 10000L))

      super$optimize(inst)
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
