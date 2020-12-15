#' @title Acquisition Function Base Class
#'
#' @description
#' Based on a surrogate model, the acquisition function encodes the preference to evaluate
#' a new point for evaluation.
#'
#' @family Acquisition Function
#'
#' @export
AcqFunction = R6Class("AcqFunction",
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    #' @field surrogate [Surrogate].
    surrogate = NULL,

    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @field search_space ([paradox::ParamSet]).
    search_space = NULL,

    #' @field codomain ([paradox::ParamSet]).
    codomain = NULL,

    #' @field direction (`character(1)`)\cr
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    direction = NULL, # optim direction of the acq function

    #' @field surrogate_max_to_min (`numeric(1)`).
    #'   Optimization direction of the objective function: 1 for minimization, -1 for maximization.
    surrogate_max_to_min = NULL, # FIXME: make this private

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param param_set ([paradox::ParamSet]).
    #' @param surrogate [Surrogate].
    #' @param direction (`character(1)`).
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    initialize = function(id, param_set, surrogate, direction) {
      self$id = assert_string(id)
      self$param_set = assert_param_set(param_set)
      self$surrogate = assert_r6(surrogate, "Surrogate")
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table()].
    #'
    #' @return [data.table::data.table()].\cr
    #' The column has to have the same name as the id of the acquisition function,
    #' because we renamed the id of the codomain.
    eval_dt = function(xdt) {
      stop("abstract")
    },

    #' @description
    #' Sets up the acquisition function.
    #'
    #' @param archive [bbotk::Archive].
    setup = function(archive) {
      # FIXME: Should we allow alternative search_space as additional argument?

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive$codomain, id = self$id, direction = self$direction)

      self$surrogate_max_to_min = mult_max_to_min(archive$codomain)

      self$search_space = archive$search_space
    },

    #' @description
    #' Update the acquisition function given an [bbotk::Archive].
    #'
    #' @param archive [bbotk::Archive].
    update = function(archive) {
      # it's okay to do nothing here
    },

    #' @description
    #' Generates an objective function for [bbotk::Optimizer].
    #'
    #' @return [bbotk::ObjectiveRFunDt]
    generate_objective = function() {
      ObjectiveRFunDt$new(
        fun = self$eval_dt,
        domain = self$search_space,
        codomain = self$codomain,
        id = self$id
      )
    }
  )
)
