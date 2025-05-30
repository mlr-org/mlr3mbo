#' @title Surrogate Model Containing Multiple Learners
#'
#' @description
#' Surrogate model containing multiple [mlr3::LearnerRegr].
#' The [mlr3::LearnerRegr] are fit on the target variables as indicated via `cols_y`.
#' Note that redundant [mlr3::LearnerRegr] must be deep clones.
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
#'     requireNamespace("rgenoud") &
#'     requireNamespace("ranger")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchMultiCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   xdt = generate_design_random(instance$search_space, n = 4)$data
#'
#'   instance$eval_batch(xdt)
#'
#'   learner1 = default_gp()
#'
#'   learner2 = default_rf()
#'
#'   surrogate = srlrn(list(learner1, learner2), archive = instance$archive)
#'
#'   surrogate$update()
#'
#'   surrogate$learner
#'
#'   surrogate$learner[["y1"]]$model
#'
#'   surrogate$learner[["y2"]]$model
#' }
SurrogateLearnerCollection = R6Class("SurrogateLearnerCollection",
  inherit = Surrogate,

  public = list(

    #' @field learner (list of [mlr3::LearnerRegr])\cr
    #'   List of [mlr3::LearnerRegr] wrapped as surrogate models.
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
    #' @param learners (list of [mlr3::LearnerRegr]).
    #' @template param_input_trafo_surrogate
    #' @template param_output_trafo_surrogate
    #' @template param_archive_surrogate
    #' @template param_cols_y_surrogate
    #' @template param_cols_x_surrogate
    initialize = function(learners, input_trafo = NULL, output_trafo = NULL, archive = NULL, cols_x = NULL, cols_y = NULL) {
      assert_learners(learners)
      addresses = map(learners, address)
      if (length(unique(addresses)) != length(addresses)) {
        stop("Redundant Learners must be unique in memory, i.e., deep clones.")
      }
      assert_learners(learners)
      for (learner in learners) {
        if (learner$predict_type != "se" && "se" %in% learner$predict_types) {
          learner$predict_type = "se"
        }
      }

      self$input_trafo = assert_r6(input_trafo, classes = "InputTrafo", null.ok = TRUE)

      self$output_trafo = assert_r6(output_trafo, classes = "OutputTrafo", null.ok = TRUE)

      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_character(cols_x, min.len = 1L, null.ok = TRUE)
      assert_character(cols_y, len = length(learners), null.ok = TRUE)

      ps = ps(
        catch_errors = p_lgl(),
        impute_method = p_fct(c("mean", "random"), default = "random")
      )
      ps$values = list(catch_errors = TRUE, impute_method = "random")

      super$initialize(learner = learners, archive = archive, cols_x = cols_x, cols_y = cols_y, param_set = ps)
    },

    #' @description
    #' Predict mean response and standard error.
    #' Returns a named list of [data.table::data.table()].
    #' Each contains the mean response and standard error for one `col_y`.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
    #'
    #' @return named list of [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)
      xdt = fix_xdt_missing(copy(xdt), cols_x = self$cols_x, archive = self$archive)
      if (!is.null(self$input_trafo)) {
        xdt = self$input_trafo$transform(xdt)
      }

      # speeding up some checks by constructing the predict task directly instead of relying on predict_newdata
      preds = lapply(self$learner, function(learner) {
        task = learner$state$train_task$clone()
        set(xdt, j = task$target_names, value = NA_real_)  # tasks only have features and the target but we have to set the target to NA
        newdata = as_data_backend(xdt)
        task$backend = newdata
        task$row_roles$use = task$backend$rownames
        pred = learner$predict(task)
        if (learner$predict_type == "se") {
          data.table(mean = pred$response, se = pred$se)
        } else {
          data.table(mean = pred$response)
        }
      })

      # slow
      #preds = lapply(self$learner, function(learner) {
      #  pred = learner$predict_newdata(newdata = xdt)
      #  if (learner$predict_type == "se") {
      #    data.table(mean = pred$response, se = pred$se)
      #  } else {
      #    data.table(mean = pred$response)
      #  }
      #})

      names(preds) = names(self$learner)
      if (!is.null(self$output_trafo) && self$output_trafo$invert_posterior) {
        preds = self$output_trafo$inverse_transform_posterior(preds)
      }
      preds
    }
  ),

  active = list(

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        paste0("(", paste0(map_chr(self$learner, function(learner) class(learner)[1L]), collapse = " | "), ")")
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @template field_n_learner_surrogate
    n_learner = function() {
      length(self$learner)
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
        unique(c(packages, unlist(map(self$learner, "packages"))))
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        Reduce(intersect, map(self$learner, "feature_types"))
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @template field_properties_surrogate
    properties = function(rhs) {
      if (missing(rhs)) {
        Reduce(intersect, map(self$learner, "properties"))
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_predict_type_surrogate
    predict_type = function(rhs) {
      if (missing(rhs)) {
        predict_types = Reduce(intersect, map(self$learner, "predict_type"))
        if (length(predict_types) == 0L) {
          stop("Learners have different active predict types.")
        }
        predict_types
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
      assert_true((length(self$cols_y) == length(self$learner)) || length(self$cols_y) == 1L)  # either as many cols_y as learner or only one
      one_to_multiple = length(self$cols_y) == 1L
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
      features = setdiff(names(xydt), self$cols_y)
      tasks = lapply(self$cols_y, function(col_y) {
        # if this turns out to be a bottleneck, we can also operate on a single task here
        task = TaskRegr$new(id = paste0("surrogate_task_", col_y), backend = xydt[, c(features, col_y), with = FALSE], target = col_y)
        task
      })
      if (one_to_multiple) {
        tasks = replicate(length(self$learner), tasks[[1L]])
      }
      pmap(list(learner = self$learner, task = tasks), .f = function(learner, task) {
        assert_learnable(task, learner = learner)
        learner$train(task)
        invisible(NULL)
      })

      if (one_to_multiple) {
        names(self$learner) = rep(self$cols_y, length(self$learner))
      } else {
        names(self$learner) = self$cols_y
      }
    },

    # Train learner with new data.
    # Operates on an asynchronous archive and performs imputation as needed.
    .update_async = function() {
      assert_true((length(self$cols_y) == length(self$learner)) || length(self$cols_y) == 1L)  # either as many cols_y as learner or only one
      one_to_multiple = length(self$cols_y) == 1L

      xydt = copy(self$archive$rush$fetch_tasks_with_state(states = c("queued", "running", "finished"))[, c(self$cols_x, self$cols_y, "state"), with = FALSE])
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
      if (self$param_set$values$impute_method == "mean") {
        walk(self$cols_y, function(col) {
          mean_y = mean(xydt[[col]], na.rm = TRUE)
          xydt[c("queued", "running"), (col) := mean_y, on = "state"]
        })
      } else if (self$param_set$values$impute_method == "random") {
        walk(self$cols_y, function(col) {
          min_y = min(xydt[[col]], na.rm = TRUE)
          max_y = max(xydt[[col]], na.rm = TRUE)
          xydt[c("queued", "running"), (col) := runif(.N, min = min_y, max = max_y), on = "state"]
        })
      }
      set(xydt, j = "state", value = NULL)

      features = setdiff(names(xydt), self$cols_y)
      tasks = lapply(self$cols_y, function(col_y) {
        # if this turns out to be a bottleneck, we can also operate on a single task here
        task = TaskRegr$new(id = paste0("surrogate_task_", col_y), backend = xydt[, c(features, col_y), with = FALSE], target = col_y)
        task
      })
      if (one_to_multiple) {
        tasks = replicate(length(self$learner), tasks[[1L]])
      }
      pmap(list(learner = self$learner, task = tasks), .f = function(learner, task) {
        assert_learnable(task, learner = learner)
        learner$train(task)
        invisible(NULL)
      })

      if (one_to_multiple) {
        names(self$learner) = rep(self$cols_y, length(self$learner))
      } else {
        names(self$learner) = self$cols_y
      }
    },

    .reset = function() {
      for (learner in self$learner) {
        learner$reset()
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        learner = map(value, function(x) x$clone(deep = TRUE)),
	input_trafo = if (is.null(value)) value else value$clone(deep = TRUE),
	output_trafo = if (is.null(value)) value else value$clone(deep = TRUE),
        .param_set = value$clone(deep = TRUE),
        .archive = value$clone(deep = TRUE),
        value
      )
    }
  )
)

