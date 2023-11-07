#' @title Linearly Transform Numeric Input Features to the Unit Cube Based on Given Boundaries
#'
#' @usage NULL
#' @name mlr_pipeops_scaleinput
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple]/[`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc]/[`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @description
#' Linearly transforms numeric input features to the unit cube based on specified lower and upper boundaries.
#' The same transformation is applied during training and prediction.
#'
#' @section Construction:
#' ```
#' PipeOpScaleInput$new(id = "scaleinput", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"scaleinput"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc].
#'
#' The output is the input [`Task`][mlr3::Task] with scaled numeric input features.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc],
#' as well as a vector for each numeric input feature as stated by the search space containing the lower and upper bound.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc], as well as:
#' * `search_space` :: [`ParamSet`][paradox::ParamSet]\cr
#'   Search space containing the numeric input features as parameters.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple]/[`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc]/[`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @export
PipeOpScaleInput = R6Class("PipeOpScaleInput",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"scaleinput"`.
    #'
    #' @param param_vals (names `list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
    initialize = function(id = "scaleinput", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("search_space", tags = c("required", "train"))
      ))
      # FIXME: maybe custom check that search space is bounded and contains at least one numeric param?
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      transformations = map(self$param_set$values$search_space$ids(), function(id) {
        c(lower = self$param_set$values$search_space$params[[id]]$lower,
          upper = self$param_set$values$search_space$params[[id]]$upper)
      })
      setNames(transformations, nm = self$param_set$values$search_space$ids())
    },

    .transform_dt = function(dt, levels) {
      if (!(all(colnames(dt) %in% self$param_set$values$search_space$ids()) && all(self$param_set$values$search_space$ids() %in% colnames(dt)))) {
        stop("Input feature IDs and search space IDs differ.")
      }
      for (id in self$param_set$values$search_space$ids()) {
        dt[[id]] = (dt[[id]] - self$state[[id]][1L]) / (self$state[[id]][2L] - self$state[[id]][1L])
      }
      dt
    }
  )
)

pipeops[["scaleinput"]] = PipeOpScaleInput
