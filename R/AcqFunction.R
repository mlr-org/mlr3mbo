#' @title Acquisition Function Base Class
#'
#' @description
#' Based on a surrogate model, the acquisition function encodes the preference to evaluate a new
#' point for evaluation.
#'
#' @family Acquisition Function
#'
#' @export
AcqFunction = R6Class("AcqFunction",
  inherit = bbotk::ObjectiveRFunDt,
  public = list(

    #' @field surrogate [Surrogate].
    surrogate = NULL,

    #' @field direction (`character(1)`)\cr
    #'   Optimization direction of the acquisition function relative to the direction of the
    #'   objective function of the [bbotk::OptimInstance].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    direction = NULL,

    #' @field surrogate_max_to_min (`-1` | `1`).
    #'   Multiplicative factor to correct for minimization or maximization of
    #'   the acquisition function.
    surrogate_max_to_min = NULL,  # FIXME: make this private

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param constants ([paradox::ParamSet]).
    #' @param surrogate ([Surrogate]).
    #' @param direction (`character(1)`).
    #' @param fun (`function(xdt)`).
    initialize = function(id, constants = ParamSet$new(), surrogate, direction, fun) {
      self$surrogate = assert_r6(surrogate, classes = "Surrogate")
      self$direction = assert_choice(direction, choices = c("same", "minimize", "maximize"))
      super$initialize(
        fun = assert_function(fun),
        domain = ParamSet$new(),  # dummy, replaced in $update()
        id = id,
        constants = constants,
        check_values = FALSE
      )
    },

    #' @description
    #' Sets up the acquisition function.
    #'
    #' @param archive ([bbotk::Archive]).
    setup = function(archive) {
      # FIXME: Should we allow alternative search_space as additional argument?
      # If we do, we need to trafo values before updating the surrogate and
      # predicting?

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive$codomain, id = self$id, direction = self$direction)
      self$surrogate_max_to_min = mult_max_to_min(archive$codomain)

      self$domain = archive$search_space$clone(deep = TRUE)
      self$domain$trafo = NULL  # FIXME: is it okay to do this?
    },

    #' @description
    #' Update the acquisition function given an [bbotk::Archive].
    #'
    #' @param archive ([bbotk::Archive]).
    update = function(archive) {
      # it's okay to do nothing here
    },

    #' @description
    #' Update the [Surrogate] model(s) with new data in an [bbotk::Archive].
    #' FIXME: probably move this simply to $update()
    #'
    #' @param archive ([bbotk::Archive]).
    update_surrogate = function(archive) {
      self$surrogate$update(xydt = archive_xy(archive), y_cols = archive$cols_y)  # update surrogate model with new data
    }
  )
)
