#' @title Acquisition Function Base Class
#'
#' @description
#' Based on a surrogate model, the acquisition function encodes the preference to evaluate a new point for evaluation.
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
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    direction = NULL,  # optim direction of the acq function

    #' @field surrogate_max_to_min (`numeric(1)`).
    #'   Optimization direction of the objective function: 1 for minimization, -1 for maximization.
    surrogate_max_to_min = NULL,  # FIXME: make this private

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param constants ([paradox::ParamSet]).
    #' @param surrogate [Surrogate].
    #' @param direction (`character(1)`).
    #' @param fun (`function(xdt)`).
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    initialize = function(id, constants = ParamSet$new(), surrogate, direction, fun) {
      self$surrogate = assert_r6(surrogate, "Surrogate")
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
      super$initialize(
        fun = assert_function(fun),
        domain = ParamSet$new(), #dummy, replaced in $update()
        id = id,
        constants = constants,
        check_values = FALSE
      )
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

      self$domain = archive$search_space$clone(deep = TRUE)
      self$domain$trafo = NULL  # FIXME: is it okay to do this?
    },

    #' @description
    #' Update the acquisition function given an [bbotk::Archive].
    #'
    #' @param archive [bbotk::Archive].
    update = function(archive) {
      # it's okay to do nothing here
    }
  )
)
