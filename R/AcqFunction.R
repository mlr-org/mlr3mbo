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
#' * setup(param_set, task, surrogate)\cr
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
    search_space = NULL,
    codomain = NULL,
    direction = NULL, # optim direction of the acq function
    mult_max_to_min = NULL, # optim direction of the obj function 1 for min, -1 for max

    initialize = function(id, param_set, surrogate, direction) {
      self$id = assert_string(id)
      self$param_set = assert_param_set(param_set)
      self$surrogate = assert_r6(surrogate, "Surrogate")
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
    },

    #' @return `data.table` \cr
    #'   The column has to have the same name as the id of the acq_fun, because we renamed the id of the codomain
    eval_dt = function(xdt) {
      stop("abstract")
    },

    
    # FIXME: Should we allow alternative search_space as additional argument?
    setup = function(archive) {

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive, direction = self$direction, id = self$id)

      self$mult_max_to_min = ifelse(archive$codomain$tags == "minimize", 1, -1)

      self$search_space = archive$search_space
      
      xydt = archive$data()
      surrogate$setup(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y)
    },

    update = function(archive) {
      xydt = archive$data()
      surrogate$update(xydt = xydt[, c(archive$cols_x, archive$cols_y), with = FALSE], y_cols = archive$cols_y)
    },

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
