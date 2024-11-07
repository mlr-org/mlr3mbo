#' @title Asynchronous Decentralized Bayesian Optimization
#' @name mlr_optimizers_adbo
#'
#' @description
#' `OptimizerADBO` class that implements Asynchronous Decentralized Bayesian Optimization (ADBO).
#' ADBO is a variant of Asynchronous Model Based Optimization (AMBO) that uses [AcqFunctionStochasticCB] with exponential lambda decay.
#'
#' @note
#' The lambda parameter of the upper confidence bound acquisition function controls the trade-off between exploration and exploitation.
#' A large lambda value leads to more exploration, while a small lambda value leads to more exploitation.
#' The initial lambda value is drawn from an exponential distribution with rate `1 / lambda`.
#' ADBO can use periodic exponential decay to reduce lambda periodically with the formula  `lambda * exp(-rate * (t %% period))`.
#' The surrogate model is always a random forest and die acquisition optimizer is random search with a budget of 10,000 evaluations.
#'
#' @section Parameters:
#' \describe{
#' \item{`lambda`}{`numeric(1)`\cr
#'   Lambda value for sampling from the exponential distribution.}
#' \item{`rate`}{`numeric(1)`\cr
#'   Rate of the exponential decay.}
#' \item{`period`}{`integer(1)`\cr
#'   Period of the exponential decay.}
#' \item{`initial_design_size`}{`integer(1)`\cr
#'   Size of the initial design.
#'   Defaults to `100`.}
#'
#' \item{`initial_design`}{`data.table::data.table()`\cr
#'   Initial design of the optimization.
#'   If `NULL`, a design of size `design_size` is generated with `design_function`.}
#' \item{`design_size`}{`integer(1)`\cr
#'   Size of the initial design.}
#' \item{`design_function`}{`character(1)`\cr
#'   Function to generate the initial design.
#'   One of `c("random", "sobol", "lhs")`.}
#' \item{`n_workers`}{`integer(1)`\cr
#'   Number of parallel workers.
#'   If `NULL`, all rush workers set with [rush::rush_plan()] are used.}
#' }
#'
#' @references
#' * `r format_bib("egele_2023")`
#'
#' @export
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
    }
  ),

  private = list(

    .optimize = function(inst) {

      self$acq_function = acqf("stochastic_cb",
        lambda = self$param_set$values$lambda,
        rate = self$param_set$values$rate,
        period = self$param_set$values$period
      )

      self$acq_optimizer = acqo(
        optimizer = opt("random_search", batch_size = 1000L),
        terminator = trm("evals", n_evals = 10000L))

      self$surrogate = default_surrogate(inst, force_rf = TRUE)

      super$.optimize(inst)
    }
  )
)

#' @include aaa.R
optimizers[["adbo"]] = OptimizerADBO
