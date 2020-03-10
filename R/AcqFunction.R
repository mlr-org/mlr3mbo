#' @title Acquisition function
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Based on a surrogate model, the acquisition function encodes the preference to evaluate
#' a new point for evaluation.
#'
#' @section Construction:
#' ```
#' acqf = AcquisitionFunction(id, opt_dir, settings, requirements)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'    Short name of the criterion.
#' * `opt_dir` :: `character(1)`\cr
#'    Should the criterion be minimized of maximized?
#'    Can be "min", "max" or "obj", where the latter means that the same direction
#' * `settings` :: named `list`\cr
#'    Control settings and constants.
#'    as specified in the objective function is taken.
#' * requirements :: named `list`\cr
#'
#' @section Fields:
#' * `surrogate` :: [mlr3::LearnerRegr]`\cr
#' * `param_set` :: [paradox::ParamSet]`\cr
#'    Feasible space to optimize over.
#' * `task` :: [mlr3::Task]`\cr
#'
#'
#' @section Methods:
#' * set_up(param_set, task, surrogate)\cr
#'   (list of [mlr3::Measure], `logical(1)`, `logical(1)`, `logical(1)`, `logical(1)`) -> [data.table::data.table()]\cr
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> `numeric(1)`\cr
#'   Evaluates all design points in `dt` with the acquisition function where each points is a row, and columns are scalar parameters.
#'
#' @export
AcqFunction = R6Class("AcqFunction", inherit = ObjectiveSO,

  public = list(
    surrogate = NULL,

    # FIXME: we somehow have to figure out the optdir adaptively?
    # FIXME: figure out way to set constants 
    # FIXME: it is weird to specify fun like this, should we not implement it as a method?
    initialize = function(objective, fun, settings, minimize, id) {
      # FIXME: do we always require a singleobj objective here? better check?
      assert_r6(objective, "ObjectiveSO")
      super$initialize(fun, objective$domain, minimize, id) # asserts minimize and id
    },

    set_up = function(domain, surrogate) {
      self$domain = assert_param_set(domain)
      self$surrogate = assert_r6(surrogate, "Surrogate")
    }
  )
)
