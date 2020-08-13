#' @title Acquisition Function
#'
#' @description
#' Based on a surrogate model, the acquisition function encodes the preference to evaluate
#' a new point for evaluation.
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

    #' @field direction (`character(1)`).
    direction = NULL, # optim direction of the acq function

    #' @field mult_max_to_min (`numeric(1)`).
    mult_max_to_min = NULL, # optim direction of the obj function 1 for min, -1 for max

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param param_set ([paradox::ParamSet]).
    #' @param surrogate [Surrogate].
    #' @param direction (`character(1)`).
    initialize = function(id, param_set, surrogate, direction) {
      self$id = assert_string(id)
      self$param_set = assert_param_set(param_set)
      self$surrogate = assert_r6(surrogate, "Surrogate")
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table]
    #'
    #' @return `data.table` \cr
    #' The column has to have the same name as the id of the acq_fun, because we
    #' renamed the id of the codomain
    eval_dt = function(xdt) {
      stop("abstract")
    },

    #' @description
    #' Sets up the acquisition function
    #'
    #' @param archive [bbotk::Archive].
    setup = function(archive) {
      # FIXME: Should we allow alternative search_space as additional argument?

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive, direction = self$direction, id = self$id)

      self$mult_max_to_min = ifelse(archive$codomain$tags == "minimize", 1, -1)

      self$search_space = archive$search_space

      xydt = archive$data()
      surrogate$setup(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y)
    },

    #' @description
    #' Update the acquisition function
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
      bbotk::ObjectiveRFunDt$new(
        fun = self$eval_dt,
        domain = self$search_space,
        codomain = self$codomain,
        id = self$id
      )
    }
  )
)
