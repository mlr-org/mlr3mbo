#' @title Surrogate Model Containing a Random Forest
#'
#' @description
#' Surrogate model containing a single Random Forest via [ranger::ranger()] from package \CRANpkg{ranger}.
#' Update and predict methods are inspired from [mlr3learners::LearnerRegrRanger] from package \CRANpkg{mlr3learners}.
#'
#' Compared to using [mlr3learners::LearnerRegrRanger] within a [SurrogateLearner] the update and predict methods of this class are much more efficient
#' as they skip many assertions and checks naturally arising when using a [SurrogateLearner] wrapping a [mlr3learners::LearnerRegrRanger].
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
#' For a description of all other parameters related to [DiceKriging::km()] directly, see the documentation of [DiceKriging::km()].
#' * The predict type hyperparameter "type" defaults to "SK" (simple kriging).
#' * The additional hyperparameter `nugget.stability` is used to overwrite the
#'   hyperparameter `nugget` with `nugget.stability * var(y)` before training to
#'   improve the numerical stability. We recommend a value of `1e-8`.
#' * The additional hyperparameter `jitter` can be set to add
#'   `N(0, [jitter])`-distributed noise to the data before prediction to avoid
#'   perfect interpolation. We recommend a value of `1e-12`.
#'
#' @export
#' @examples
#' if (requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
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
#'   surrogate = SurrogateGP$new(archive = instance$archive)
#'   surrogate$param_set$set_values(
#'     covtype = "matern5_2",
#'     optim.method = "gen",
#'     control = list(trace = FALSE),
#'     nugget.stability = 10^-8
#'   )
#'
#'   surrogate$update()
#'
#'   surrogate$learner$model
#' }
SurrogateRF = R6Class("SurrogateRF",
  inherit = Surrogate,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_archive_surrogate
    #' @template param_col_y_surrogate
    #' @template param_cols_x_surrogate
    initialize = function(archive = NULL, cols_x = NULL, col_y = NULL) {
      assert_r6(archive, classes = "Archive", null.ok = TRUE)

      assert_character(cols_x, min.len = 1L, null.ok = TRUE)
      assert_string(col_y, null.ok = TRUE)

      # https://github.com/mlr-org/mlr3learners/blob/51c15755438078fc99b11a9ca0147c7f2dbb96d8/R/LearnerRegrKM.R#L34
      ps = ps(
        always.split.variables       = p_uty(tags = "train"),
        holdout                      = p_lgl(default = FALSE, tags = "train"),
        importance                   = p_fct(c("none", "impurity", "impurity_corrected", "permutation"), tags = "train"),
        keep.inbag                   = p_lgl(default = FALSE, tags = "train"),
        max.depth                    = p_int(default = NULL, lower = 1L, special_vals = list(NULL), tags = "train"),
        min.bucket                   = p_int(1L, default = 1L, tags = "train"),
        min.node.size                = p_int(1L, default = 5L, special_vals = list(NULL), tags = "train"),
        mtry                         = p_int(lower = 1L, special_vals = list(NULL), tags = "train"),
        mtry.ratio                   = p_dbl(lower = 0, upper = 1, tags = "train"),
        na.action                    = p_fct(c("na.learn", "na.omit", "na.fail"), default = "na.learn", tags = "train"),
        node.stats                   = p_lgl(default = FALSE, tags = "train"),
        num.random.splits            = p_int(1L, default = 1L, tags = "train", depends = quote(splitrule == "extratrees")),
        num.threads                  = p_int(1L, default = 1L, tags = c("train", "predict", "threads")),
        num.trees                    = p_int(1L, default = 500L, tags = c("train", "predict", "hotstart")),
        oob.error                    = p_lgl(default = TRUE, tags = "train"),
        poisson.tau                  = p_dbl(default = 1, tags = "train", depends = quote(splitrule == "poisson")),
        regularization.factor        = p_uty(default = 1, tags = "train"),
        regularization.usedepth      = p_lgl(default = FALSE, tags = "train"),
        replace                      = p_lgl(default = TRUE, tags = "train"),
        respect.unordered.factors    = p_fct(c("ignore", "order", "partition"), tags = "train"),
        sample.fraction              = p_dbl(0L, 1L, tags = "train"),
        save.memory                  = p_lgl(default = FALSE, tags = "train"),
        scale.permutation.importance = p_lgl(default = FALSE, tags = "train", depends = quote(importance == "permutation")),
        se.method                    = p_fct(c("jack", "infjack", "simple", "law_of_total_variance"), default = "infjack", tags = "predict"),
        seed                         = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "predict")),
        split.select.weights         = p_uty(default = NULL, tags = "train"),
        splitrule                    = p_fct(c("variance", "extratrees", "maxstat", "beta", "poisson"), default = "variance", tags = "train"),
        verbose                      = p_lgl(default = TRUE, tags = c("train", "predict")),
        write.forest                 = p_lgl(default = TRUE, tags = "train"),
        catch_errors                 = p_lgl(tags = "required"),
        impute_method                = p_fct(c("mean", "random"), tags = "required")
      )
      ps$values = list(catch_errors = TRUE, impute_method = "random")

      super$initialize(learner = list(), archive = archive, cols_x = cols_x, cols_y = col_y, param_set = ps)
    },

    #' @description
    #' Predict mean response and standard error.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data. One row per observation.
    #'
    #' @return [data.table::data.table()] with the columns `mean` and `se`.
    predict = function(xdt) {
      assert_xdt(xdt)
      newdata = as_numeric_matrix(fix_xdt_missing(xdt, cols_x = self$cols_x, archive = self$archive))
      pv = self$param_set$get_values(tags = "predict")

      p = invoke(predict, private$.model,
        data = newdata,
        type = self$predict_type,
        quantiles = private$.quantiles,
        .args = pv)

      list(mean = p$predictions, se = p$se)
    }
  ),

  active = list(

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        "ranger::ranger"
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
        "DiceKriging"
      } else {
        stop("$packages is read-only.")
      }
    },

    #' @template field_feature_types_surrogate
    feature_types = function(rhs) {
      if (missing(rhs)) {
        c("logical", "integer", "numeric")
      } else {
        stop("$feature_types is read-only.")
      }
    },

    #' @template field_properties_surrogate
    properties = function(rhs) {
      if (missing(rhs)) {
        character(0L)
      } else {
        stop("$properties is read-only.")
      }
    },

    #' @template field_predict_type_surrogate
    predict_type = function(rhs) {
      if (missing(rhs)) {
        "se"
      } else {
        stop("$predict_type is read-only.")
      }
    }
  ),

  private = list(

    .model = NULL,

    # Train learner with new data.
    .update = function() {
      pv = self$param_set$get_values(tags = "train")
      pv = convert_ratio(pv, "mtry", "mtry.ratio", length(self$cols_x))
      design = as_numeric_matrix(self$archive$data[, c(self$cols_x, self$cols_y), with = FALSE])

      model = invoke(ranger::ranger,
        dependent.variable.name = self$cols_y,
        data = design,
        .args = pv
      )

      private$.model = if (isTRUE(self$param_set$values$se.method %in% c("simple", "law_of_total_variance"))) {
        prediction_nodes = mlr3misc::invoke(predict, model, data = design, type = "terminalNodes", .args = pv[setdiff(names(pv), "se.method")], predict.all = TRUE)
        storage.mode(prediction_nodes$predictions) = "integer"
        mu_sigma = .Call("c_ranger_mu_sigma", model, prediction_nodes$predictions, self$archive$data[[self$cols_y]])
        list(model = model, mu_sigma = mu_sigma)
      } else {
        list(model = model)
      }

      self$learner$model = private$.model
      invisible(NULL)
    },

    # Train learner with new data.
    # Operates on an asynchronous archive and performs imputation as needed.
    .update_async = function() {
      xydt = self$archive$rush$fetch_tasks_with_state(states = c("queued", "running", "finished"))[, c(self$cols_x, self$cols_y, "state"), with = FALSE]

      if (self$param_set$values$impute_method == "mean") {
        mean_y = mean(xydt[[self$cols_y]], na.rm = TRUE)
        xydt[c("queued", "running"), (self$cols_y) := mean_y, on = "state"]
      } else if (self$param_set$values$impute_method == "random") {
        min_y = min(xydt[[self$cols_y]], na.rm = TRUE)
        max_y = max(xydt[[self$cols_y]], na.rm = TRUE)
        xydt[c("queued", "running"), (self$cols_y) := runif(.N, min = min_y, max = max_y), on = "state"]
      }
      set(xydt, j = "state", value = NULL)

      pv = self$param_set$get_values(tags = "train")
      pv = convert_ratio(pv, "mtry", "mtry.ratio", length(self$cols_x))
      design = as_numeric_matrix(xydt[, c(self$cols_x, self$cols_y), with = FALSE])

      model = invoke(ranger::ranger,
        dependent.variable.name = self$cols_y,
        data = design,
        .args = pv
      )

      private$.model = if (isTRUE(self$param_set$values$se.method %in% c("simple", "law_of_total_variance"))) {
        prediction_nodes = mlr3misc::invoke(predict, model, data = design, type = "terminalNodes", .args = pv[setdiff(names(pv), "se.method")], predict.all = TRUE)
        storage.mode(prediction_nodes$predictions) = "integer"
        mu_sigma = .Call("c_ranger_mu_sigma", model, prediction_nodes$predictions, xydt[[self$cols_y]])
        list(model = model, mu_sigma = mu_sigma)
      } else {
        list(model = model)
      }

      self$learner$model = private$.model
      invisible(NULL)
    },

    .reset = function() {
      self$learner = list()
      private$.model = NULL
    },

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        .archive = value$clone(deep = TRUE),
        value
      )
    }
  )
)

# https://github.com/mlr-org/mlr3learners/blob/51c15755438078fc99b11a9ca0147c7f2dbb96d8/R/helpers.R#L16C1-L16C18
as_numeric_matrix = function(x) {
  x = as.matrix(x)
  if (is.logical(x)) {
    storage.mode(x) = "double"
  }
  x
}

convert_ratio = function(pv, target, ratio, n) {
  switch(to_decimal(c(target, ratio) %in% names(pv)) + 1L,
    # !mtry && !mtry.ratio
    pv,

    # !mtry && mtry.ratio
    {
      pv[[target]] = max(ceiling(pv[[ratio]] * n), 1)
      remove_named(pv, ratio)
    },


    # mtry && !mtry.ratio
    pv,

    # mtry && mtry.ratio
    stopf("Hyperparameters '%s' and '%s' are mutually exclusive", target, ratio)
  )
}
