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
#'   (list of [Measure], `logical(1)`, `logical(1)`, `logical(1)`, `logical(1)`) -> [data.table::data.table()]\cr
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> `numeric(1)`\cr
#'   Evaluates all design points in `dt` with the acquisition function where each points is a row, and columns are scalar parameters.
#'
#' @export
AcqFunction = R6Class("AcqFunction",

  public = list(
    id = NULL,
    settings = NULL,
    surrogate = NULL,
    opt_dir = NULL,
    param_set = NULL,
    task = NULL,

    initialize = function(id, opt_dir, settings, requirements) {
      self$id = assert_string(id)
      self$opt_dir = assert_choice(opt_dir, c("min", "max", "obj"))
      self$settings = settings
    },

    set_up = function(param_set, task, surrogate) {
      self$param_set = param_set
      self$task = task
      self$surrogate = surrogate
    },
   
    eval_batch = function(dt) stop("abstract"),

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(self$format())
      catf("opt_dir: %s", self$opt_dir)
      print(self$param_set)
      print(self$terminator)
      print(self$archive)
    }
  )
)
