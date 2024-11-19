#' @title Surrogate Model Containing a Gaussian Process
#'
#' @description
#' Surrogate model containing a single Gaussian Process via [DiceKriging::km()] from package \CRANpkg{DiceKriging}.
#' Update and predict methods are inspired from [mlr3learners::LearnerRegrKM] from package \CRANpkg{mlr3learners}.
#'
#' Compared to using [mlr3learners::LearnerRegrKM] within a [SurrogateLeaner] the update and predict methods of this class are much more efficient
#' as they skip many assertions and checks naturally arising when using a [SurrogateLeaner] wrapping a [mlr3learners::LearnerRegrKM].
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
SurrogateGP = R6Class("SurrogateGP",
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
        bias.correct     = p_lgl(default = FALSE, tags = "predict"),
        checkNames       = p_lgl(default = TRUE, tags = "predict"),
        coef.cov         = p_uty(default = NULL, tags = "train"),
        coef.trend       = p_uty(default = NULL, tags = "train"),
        coef.var         = p_uty(default = NULL, tags = "train"),
        control          = p_uty(default = NULL, tags = "train"),
        # cov.compute      = p_lgl(default = TRUE, tags = "predict"),
        covtype          = p_fct(c("gauss", "matern5_2", "matern3_2", "exp", "powexp"), default = "matern5_2", tags = "train"),
        estim.method     = p_fct(c("MLE", "LOO"), default = "MLE", tags = "train"),
        gr               = p_lgl(default = TRUE, tags = "train"),
        iso              = p_lgl(default = FALSE, tags = "train"),
        jitter           = p_dbl(0, default = 0, tags = "predict"),
        kernel           = p_uty(default = NULL, tags = "train"),
        knots            = p_uty(default = NULL, tags = "train", depends = quote(scaling == TRUE)),
        light.return     = p_lgl(default = FALSE, tags = "predict"),
        lower            = p_uty(default = NULL, tags = "train"),
        multistart       = p_int(default = 1, tags = "train", depends = quote(optim.method == "BFGS")),
        noise.var        = p_uty(default = NULL, tags = "train"),
        nugget           = p_dbl(tags = "train"),
        nugget.estim     = p_lgl(default = FALSE, tags = "train"),
        nugget.stability = p_dbl(0, default = 0, tags = "train"),
        optim.method     = p_fct(c("BFGS", "gen"), default = "BFGS", tags = "train"),
        parinit          = p_uty(default = NULL, tags = "train"),
        penalty          = p_uty(default = NULL, tags = "train"),
        scaling          = p_lgl(default = FALSE, tags = "train"),
        # se.compute       = p_lgl(default = TRUE, tags = "predict"),
        type             = p_fct(c("SK", "UK"), default = "SK", tags = "predict"),
        upper            = p_uty(default = NULL, tags = "train"),
        catch_errors     = p_lgl(tags = "required"),
        impute_method    = p_fct(c("mean", "random"), tags = "required")
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

      jitter = pv$jitter
      if (!is.null(jitter) && jitter > 0) {
        newdata = newdata + stats::rnorm(length(newdata), mean = 0, sd = jitter)
      }

      p = invoke(DiceKriging::predict.km,
        private$.model,
        newdata = newdata,
        type = if (is.null(pv$type)) "SK" else pv$type,
        se.compute = TRUE,
        .args = remove_named(pv, "jitter")
      )

      data.table(mean = p$mean, se = p$sd)
    }
  ),

  active = list(

    #' @template field_print_id
    print_id = function(rhs) {
      if (missing(rhs)) {
        "DiceKriging::km"
      } else {
        stop("$print_id is read-only.")
      }
    },

    #' @template field_assert_insample_perf_surrogate
    assert_insample_perf = function(rhs) {
      stopf("Not implemented.")
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
      design = as_numeric_matrix(self$archive$data[, self$cols_x, with = FALSE])
      response = self$archive$data[[self$cols_y]]

      if (!is.null(pv$optim.method) && pv$optim.method == "gen" && !requireNamespace("rgenoud", quietly = TRUE)) {
        stopf("The 'rgenoud' package is required for optimization method 'gen'.")
      }

      ns = pv$nugget.stability
      if (!is.null(ns)) {
        pv$nugget = if (ns == 0) 0 else ns * stats::var(response)
      }

      private$.model = invoke(DiceKriging::km,
        response = response,
        design = design,
        control = pv$control,
        .args = remove_named(pv, c("control", "nugget.stability"))
      )
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
      design = as_numeric_matrix(xydt[, self$cols_x, with = FALSE])
      response = xydt[[self$cols_y]]

      if (!is.null(pv$optim.method) && pv$optim.method == "gen" && !requireNamespace("rgenoud", quietly = TRUE)) {
        stopf("The 'rgenoud' package is required for optimization method 'gen'.")
      }

      ns = pv$nugget.stability
      if (!is.null(ns)) {
        pv$nugget = if (ns == 0) 0 else ns * stats::var(response)
      }

      private$.model = invoke(DiceKriging::km,
        response = response,
        design = design,
        control = pv$control,
        .args = remove_named(pv, c("control", "nugget.stability"))
      )
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
