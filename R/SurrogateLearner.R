#' @title Surrogate Model Containing a Single Learner
#'
#' @description
#' Surrogate model containing a single [mlr3::LearnerRegr].
#'
#' @section Parameters:
#' \describe{
#' \item{`catch_errors`}{`logical(1)`\cr
#'   Should errors during updating the surrogate be caught and propagated to the `loop_function` which can then handle
#'   the failed acquisition function optimization (as a result of the failed surrogate) appropriately by, e.g., proposing a randomly sampled point for evaluation?
#'   Default is `TRUE`.
#' }
#' \item{`impute_method`}{`character(1)`\cr
#'   Method to impute missing values in the case of updating on an asynchronous [bbotk::ArchiveAsync] with pending evaluations.
#'   Can be `"mean"` to use mean imputation or `"random"` to sample values uniformly at random between the empirical minimum and maximum.
#'   Default is `"random"`.
#' }
#' }
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   xdt = generate_design_random(instance$search_space, n = 4)$data
#'
#'   instance$eval_batch(xdt)
#'
#'   learner = default_gp()
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   surrogate$update()
#'
#'   surrogate$learner$model
#' }
SurrogateLearner = R6Class("SurrogateLearner",
  inherit = Surrogate,

  public = list(

    #' @field learner ([mlr3::LearnerRegr])\cr
    #'   [mlr3::LearnerRegr] wrapped as a surrogate model.
    learner = NULL,

    #' @field input_trafo ([InputTrafo])\cr
    #'   Input transformation.
    input_trafo = NULL,

    #' @field output_trafo ([OutputTrafo])\cr
    #'   Output transformation.
    output_trafo = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr]).
    #' @template param_input_trafo_surrogate
    #' @template param_output_trafo_surrogate
    #' @template param_archive_surrogate
    #' @template param_col_y_surrogate
    #' @template param_cols_x_surrogate
    initialize = function(learner, input_trafo = NULL, output_trafo = NULL, archive = NULL, cols_x = NULL, col_y = NULL) {
      assert_learner(learner)
      if (learner$predict_type != "se" && "se" %in% learner$predict_types) {
        learner$predict_type = "se"
      }

      self$input_trafo = assert_r6(input_trafo, classes = "InputTrafo", null.ok = TRUE)

      self$output_trafo = assert_r6(output_trafo, classes = "OutputTrafo", null.ok = TRUE)

      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_character(cols_x, min.len = 1L, null.ok = TRUE)
      assert_string(col_y, null.ok = TRUE)

      ps = ps(
        catch_errors = p_lgl(),
        impute_method = p_fct(c("mean", "random"), default = "random")
      )
      ps$values = list(catch_errors = TRUE, impute_method = "random")

      private$.predict_names = if (learner$predict_type == "se") {
        c("mean", "se")
      } else {
        "mean"
      }
      super$initialize(learner = learner, archive = archive, cols_x = cols_x, cols_y = col_y, param_set = ps)
    },

    #' @description
    #' Predict mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      # assert_xdt(xdt)

      # xdt = fix_xdt_missing(copy(xdt), cols_x = self$cols_x, archive = self$archive)

      if (!is.null(self$input_trafo)) {
        xdt = self$input_trafo$transform(xdt)
      }

      if (!inherits(self$learner, "GraphLearner")) {
        pred = self$learner$predict_newdata_fast(xdt)
        pred = set_names(pred, private$.predict_names)
      } else {
        # speeding up some checks by constructing the predict task directly instead of relying on predict_newdata
        task = self$learner$state$train_task$clone()
        set(xdt, j = task$target_names, value = NA_real_)  # tasks only have features and the target but we have to set the target to NA
        newdata = as_data_backend(xdt)
        task$backend = newdata
        task$row_roles$use = task$backend$rownames
        pred = self$learner$predict(task)

        pred = if (self$learner$predict_type == "se") {
          list(mean = pred$response, se = pred$se)
        } else {
          list(mean = pred$response)
        }
      }

      if (!is.null(self$output_trafo) && self$output_trafo$invert_posterior) {
        pred = self$output_trafo$inverse_transform_posterior(as.data.table(pred))
      }
      pred
    }
  ),

  active = list(

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        class(self$learner)[1L]
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @template field_n_learner_surrogate
    n_learner = function() {
      1L
    },

    #' @template field_packages_surrogate
    packages = function(rhs) {
      if (missing(rhs)) {
        packages = character(0L)
        if (!is.null(self$input_trafo)) {
          packages = c(packages, self$input_trafo$packages)
        }
        if (!is.null(self$output_trafo)) {
          packages = c(packages, self$output_trafo$packages)
        }
        unique(c(packages, self$learner$packages))
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        self$learner$feature_types
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @template field_properties_surrogate
    properties = function(rhs) {
      if (missing(rhs)) {
        self$learner$properties
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_predict_type_surrogate
    predict_type = function(rhs) {
      if (missing(rhs)) {
        self$learner$predict_type
      } else {
        stop("$predict_type is read-only. To change it, modify $predict_type of the learner directly.")
      }
    },

    #' @template field_output_trafo_must_be_considered
    output_trafo_must_be_considered = function(rhs) {
      if (missing(rhs)) {
        !is.null(self$output_trafo) && !self$output_trafo$invert_posterior
      } else {
        stop("$output_trafo_must_be_considered is read-only.")
      }
    }
  ),

  private = list(

    # Train learner with new data.
    .update = function() {
      xydt = copy(self$archive$data[, c(self$cols_x, self$cols_y), with = FALSE])
      if (!is.null(self$input_trafo)) {
        self$input_trafo$cols_x = self$cols_x
        self$input_trafo$search_space = self$archive$search_space
        self$input_trafo$update(xydt)
        xydt = self$input_trafo$transform(xydt)
      }
      if (!is.null(self$output_trafo)) {
        self$output_trafo$cols_y = self$cols_y
        self$output_trafo$max_to_min = surrogate_mult_max_to_min(self)
        self$output_trafo$update(xydt)
        xydt = self$output_trafo$transform(xydt)
      }
      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = self$cols_y)
      assert_learnable(task, learner = self$learner)
      self$learner$train(task)
    },

    # Train learner with new data.
    # Operates on an asynchronous archive and performs imputation as needed.
    .update_async = function() {
      xydt = copy(self$archive$rush$fetch_tasks_with_state(states = c("queued", "running", "finished"))[, c(self$cols_x, self$cols_y, "state"), with = FALSE])

      if (self$param_set$values$impute_method == "mean") {
        mean_y = mean(xydt[[self$cols_y]], na.rm = TRUE)
        xydt[c("queued", "running"), (self$cols_y) := mean_y, on = "state"]
      } else if (self$param_set$values$impute_method == "random") {
        min_y = min(xydt[[self$cols_y]], na.rm = TRUE)
        max_y = max(xydt[[self$cols_y]], na.rm = TRUE)
        print(min_y)
        print(max_y)
        xydt[c("queued", "running"), (self$cols_y) := runif(.N, min = min_y, max = max_y), on = "state"]
      }
      set(xydt, j = "state", value = NULL)

      if (!is.null(self$input_trafo)) {
        self$input_trafo$cols_x = self$cols_x
        self$input_trafo$search_space = self$archive$search_space
        self$input_trafo$update(xydt)
        xydt = self$input_trafo$transform(xydt)
      }
      if (!is.null(self$output_trafo)) {
        self$output_trafo$cols_y = self$cols_y
        self$output_trafo$max_to_min = surrogate_mult_max_to_min(self)
        self$output_trafo$update(xydt)
        xydt = self$output_trafo$transform(xydt)
      }

      task = TaskRegr$new(id = "surrogate_task", backend = xydt, target = self$cols_y)
      assert_learnable(task, learner = self$learner)
      self$learner$train(task)
    },

    .reset = function() {
      self$learner$reset()
    },

    deep_clone = function(name, value) {
      switch(name,
        learner = value$clone(deep = TRUE),
        input_trafo = if (is.null(value)) value else value$clone(deep = TRUE),
        output_trafo = if (is.null(value)) value else value$clone(deep = TRUE),
        .param_set = value$clone(deep = TRUE),
        .archive = value$clone(deep = TRUE),
        value
      )
    },

    .predict_names = NULL
  )
)

