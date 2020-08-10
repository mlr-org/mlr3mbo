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
AcqFunction = R6Class("AcqFunction",
  
  public = list(

    id = NULL,
    surrogate = NULL,
    param_set = NULL,
    domain = NULL,
    codomain = NULL,

    initialize = function(id, param_set) {
      self$id = assert_string(id)
      self$param_set = assert_param_set(param_set)
      super$initialize(id, properties = properties, domain = domain, codomain = codomain, check_values = FALSE)
      self$param_set = assert_param_set(param_set)
    },

    eval_dt = function(xdt) {
      stop("abstract")
    },

    set_up = function(surrogate, domain, codomain) {
      self$surrogate = assert_r6(surrogate, "Surrogate")
      self$domain = assert_param_set(domain)
      self$codomain = assert_param_set(codomain)
    },

    generate_objective = function() {
      bbotk::ObjectiveRFunDt$new(
        fun = self$eval_dt,
        domain = self$domain,
        codomain = self$codomain,
        id = self$id
      )
    }
  )
)
