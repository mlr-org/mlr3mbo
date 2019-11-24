#' @title Black-Box objective function
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Specifies a black-box function and archive for optimizers to
#' act upon.
#' It allows the basic operations of querying the objective
#' at design points (`$eval_batch()`), storing the evaluations in an internal archive
#' and querying the archive (`$archive()`).
#'
#' Evaluations of points are performed in batches.
#' Before and after a batch is evaluated, the [Terminator] is queried for the remaining budget.
#' If the available budget is exhausted, an exception is raised, and no further evaluations can be performed from this point on.
#'
#' The optimizer is also supposed to store its final result, consisting of a list of optimal values
#' and stimated performance values, by calling the method `instance$assign_result`.
#'
#' @section Construction:
#' ```
#' inst = Objective$new param_set, terminator)
#' ```
#' This defines the resampled performance of a learner on a task, a feasibility region
#' for the parameters the tuner is supposed to optimize, and a termination criterion.
#'
#' * `param_set` :: [paradox::ParamSet].
#' * `terminator` :: [Terminator].
#'
#' @section Fields:
#' * `param_set` :: [paradox::ParamSet]; from construction.
#' * `terminator` :: [Terminator]; from construction.
#' * `n_evals` :: `integer(1)`\cr
#'   Number of configuration evaluations stored in the container.
#' * `start_time` :: `POSIXct(1)`\cr
#'   Time the tuning was started.
#'   This is set in the beginning of `$tune()` of [mlr3tuning::Tuner].
#' * `result` :: named `list()`\cr
#'   Result of the tuning, i.e., the optimal configuration and its estimated performance:
#'   * `"perf"`: Named vector of estimated performance values of the best configuration found.
#'   * `"tune_x"`: Named list of optimal hyperparameter settings, without potential `trafo` function applied.
#'   * `"params"`: Named list of optimal hyperparameter settings, similar to `tune_x`, but with potential `trafo` function applied.
#'     Also, if the learner had some extra parameters statically set before tuning, these are included here.
#'
#' @section Methods:
#' * `eval_batch(dt)`\cr
#'   [data.table::data.table()] -> named `list()`\cr
#'   Evaluates all hyperparameter configurations in `dt` through resampling, where each configuration is a row, and columns are scalar parameters.
#'   Updates the internal [mlr3::BenchmarkResult] `$bmr` by reference, and returns a named list with the following elements:
#'   * `"batch_nr"`: Number of the new batch.
#'     This number is calculated in an auto-increment fashion and also stored inside the [mlr3::BenchmarkResult] as column `batch_nr`
#'   * `"perf"`: A [data.table::data.table()] of evaluated performances for each row of the `dt`.
#'     Has the same number of rows as `dt`, and the same number of columns as length of `measures`.
#'     Columns are named with measure-IDs. A cell entry is the (aggregated) performance of that configuration for that measure.
#'
#'   Before and after each batch-evaluation, the [Terminator] is checked, and if it is positive, an exception of class `terminated_error` is raised.
#'   This function should be internally called by the tuner.
#'
#' * `best(measure = NULL)`\cr
#'   ([mlr3::Measure], `character(1)`) -> [mlr3::ResampleResult]\cr
#'   Queries the [mlr3::BenchmarkResult] for the best [mlr3::ResampleResult] according to `measure` (default is the first measure in `$measures`).
#'   In case of ties, one of the tied values is selected randomly.
#'
#' * `archive(unnest = "no")`\cr
#'   `character(1)` -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, similar to the one returned by [mlr3::benchmark()]'s `$aggregate()` method.
#'   Some interesting columns of this table are:
#'   * All evaluated measures are included as numeric columns, named with their measure ID.
#'   * `tune_x`: A list column that contains the parameter settings the tuner evaluated, without potential `trafo` applied.
#'   * `params`: A list column that contains the parameter settings that were actually used in the learner.
#'      Similar to `tune_x`, but with potential `trafo` applied.
#'      Also, if the learner had some extra parameters statically set before tuning, these are included here.
#'   `unnest` can have the values `"no"`, `"tune_x"` or `"params"`. If it is not set to `"no"`, settings of the respective list-column
#'   are stored in separate columns instead of the list-column, and dependent, inactive parameters are encoded with `NA`.
#'
#' * `assign_result(tune_x, perf)`\cr
#'   (`list`, `numeric`) -> `NULL`\cr
#'   The tuner writes the best found list of settings and estimated performance values here. For internal use.
#'   * `tune_x`: Must be a named list of settings only of parameters from `param_set` and be feasible, untransformed.
#'   * `perf` : Must be a named numeric vector of performance measures, named with performance IDs, regarding all elements in `measures`.
#'
#' @export
Objective = R6Class("Objective",

  public = list(
    fun = NULL,
    minimize = NULL,
    param_set = NULL,
    archive = NULL,
    terminator = NULL,

    initialize = function(fun, param_set, terminator, minimize = TRUE) {
      self$fun = assert_function(fun)
      self$param_set = assert_param_set(param_set)
      self$terminator = assert_r6(terminator, "Terminator")
      self$minimize = assert_flag(minimize)
      self$archive = Archive$new(ps)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(self$format())
      print(self$param_set)
      print(self$terminator)
      print(self$archive)
    },

    # evaluates all points in a design
    # possibly transforms the data before using the trafo from self$param set
    eval_batch = function(dt) {
      # dt can contain missings because of non-fulfilled dependencies
      assert_data_table(dt, any.missing = TRUE, min.rows = 1L, min.cols = 1L)

      # this checks the validity of dt lines in the paramset
      design = Design$new(self$param_set, dt, remove_dupl = FALSE)

      lg$info("Evaluating %i points", nrow(dt))
      lg$info(capture.output(print(dt, class = FALSE, row.names = FALSE, print.keys = FALSE)))

      # convert configs to lists and remove non-satisfied deps
      parlist_trafoed = design$transpose(trafo = TRUE, filter_na = TRUE)
      parlist_untrafoed = design$transpose(trafo = FALSE, filter_na = TRUE)

      # eval
      # FIXME: allow this in parallel and encapsulated
      # FIXME: allow multiple values here? or subclass with MOO objective?
      y = vnapply(parlist_trafoed, self$fun)
      assert_numeric(y, len = nrow(dt))

      # store evaluated results
      archive2 = dt
      archive2$y = y
      self$archive$add(archive2)
    },

    assign_result = function(tune_x, perf) {
      # result tune_x must be feasible for paramset
      self$param_set$assert(tune_x)
      # result perf must be numeric and cover all measures
      assert_numeric(perf)
      assert_names(names(perf), permutation.of = ids(self$measures))
      private$.result = list(tune_x = tune_x, perf = perf)
    }
  ),

  active = list(
    n_evals = function() nrow(self$archive),

    result = function() {
      tune_x = private$.result$tune_x
      perf = private$.result$perf
      # if ps has no trafo, just use the normal config
      trafo = if (is.null(self$param_set$trafo)) identity else self$param_set$trafo
      params = trafo(tune_x)
      params = insert_named(self$learner$param_set$values, params)
      list(tune_x = tune_x, params = params, perf = perf)
    }
  ),

  private = list(
    .result = NULL
  )
)
