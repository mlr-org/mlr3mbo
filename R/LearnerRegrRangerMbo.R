#' @title Custom Ranger Regression Learner for Model Based Optimization
#'
#' @name mlr_learners_regr.ranger
#'
#' @description
#' Random regression forest.
#' Calls [ranger::ranger()] from package \CRANpkg{ranger}.
#'
#' @export
LearnerRegrRangerMbo = R6Class("LearnerRegrRangerMbo",
  inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        alpha                        = p_dbl(default = 0.5, tags = "train", depends = quote(splitrule == "maxstat")),
        always.split.variables       = p_uty(tags = "train"),
        holdout                      = p_lgl(default = FALSE, tags = "train"),
        importance                   = p_fct(c("none", "impurity", "impurity_corrected", "permutation"), tags = "train"),
        keep.inbag                   = p_lgl(default = FALSE, tags = "train"),
        max.depth                    = p_int(default = NULL, lower = 0L, special_vals = list(NULL), tags = "train"),
        min.bucket                   = p_int(1L, default = 1L, tags = "train"),
        min.node.size                = p_int(1L, default = 5L, special_vals = list(NULL), tags = "train"),
        minprop                      = p_dbl(default = 0.1, tags = "train", depends = quote(splitrule == "maxstat")),
        mtry                         = p_int(lower = 1L, special_vals = list(NULL), tags = "train"),
        mtry.ratio                   = p_dbl(lower = 0, upper = 1, tags = "train"),
        node.stats                   = p_lgl(default = FALSE, tags = "train"),
        num.random.splits            = p_int(1L, default = 1L, tags = "train", depends = quote(splitrule == "extratrees")),
        num.threads                  = p_int(1L, default = 1L, tags = c("train", "predict", "threads")),
        num.trees                    = p_int(1L, default = 500L, tags = c("train", "predict", "hotstart")),
        oob.error                    = p_lgl(default = TRUE, tags = "train"),
        quantreg                     = p_lgl(default = FALSE, tags = "train"),
        regularization.factor        = p_uty(default = 1, tags = "train"),
        regularization.usedepth      = p_lgl(default = FALSE, tags = "train"),
        replace                      = p_lgl(default = TRUE, tags = "train"),
        respect.unordered.factors    = p_fct(c("ignore", "order", "partition"), default = "ignore", tags = "train"),
        sample.fraction              = p_dbl(0L, 1L, tags = "train"),
        save.memory                  = p_lgl(default = FALSE, tags = "train"),
        scale.permutation.importance = p_lgl(default = FALSE, tags = "train", depends = quote(importance == "permutation")),
        se.method                    = p_fct(c("jack", "infjack", "simple", "law_of_total_variance"), default = "infjack", tags = c("train", "predict")), # FIXME: only works if predict_type == "se"
        seed                         = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "predict")),
        split.select.weights         = p_uty(default = NULL, tags = "train"),
        splitrule                    = p_fct(c("variance", "extratrees", "maxstat"), default = "variance", tags = "train"),
        verbose                      = p_lgl(default = TRUE, tags = c("train", "predict")),
        write.forest                 = p_lgl(default = TRUE, tags = "train")
      )

      ps$values = list(num.threads = 1L)

      super$initialize(
        id = "regr.ranger_mbo",
        param_set = ps,
        predict_types = c("response", "se"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "importance", "oob_error", "hotstart_backward"),
        packages = c("mlr3learners", "ranger"),
        label = "Custom MBO Random Forest",
        man = "mlr3mbo::mlr_learners_regr.ranger_mbo"
      )
    },

    #' @description
    #' The importance scores are extracted from the model slot `variable.importance`.
    #' Parameter `importance.mode` must be set to `"impurity"`, `"impurity_corrected"`, or
    #' `"permutation"`
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (self$model$importance.mode == "none") {
        stopf("No importance stored")
      }

      sort(self$model$variable.importance, decreasing = TRUE)
    },

    #' @description
    #' The out-of-bag error, extracted from model slot `prediction.error`.
    #'
    #' @return `numeric(1)`.
    oob_error = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      self$model$prediction.error
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      pv = mlr3learners:::convert_ratio(pv, "mtry", "mtry.ratio", length(task$feature_names))
      if ("se.method" %in% names(pv)) {
        if (self$predict_type != "se") {
          stop('$predict_type must be "se" if "se.method" is set to "simple" or "law_of_total_variance".')
        }
        pv[["se.method"]] = NULL
      }

      if (self$predict_type == "se") {
        pv$keep.inbag = TRUE # nolint
      }

      model = mlr3misc::invoke(ranger::ranger,
        dependent.variable.name = task$target_names,
        data = task$data(),
        case.weights = task$weights$weight,
        .args = pv
      )

      if (isTRUE(self$param_set$get_values()[["se.method"]] %in% c("simple", "law_of_total_variance"))) {
        data = mlr3learners:::ordered_features(task, self)
        prediction_nodes = mlr3misc::invoke(predict, model, data = data, type = "terminalNodes", .args = pv[setdiff(names(pv), "se.method")], predict.all = TRUE)
        y = task$data(cols = task$target_names)[[1L]]
        observation_node_table = as.matrix(prediction_nodes$predictions)
        storage.mode(observation_node_table) = "integer"
        mu_sigma2_per_node_per_tree = .Call("c_mu_sigma2_per_node_per_tree", observation_node_table, y)
        list(model = model, mu_sigma2_per_node_per_tree = mu_sigma2_per_node_per_tree)
      } else {
        list(model = model)
      }
    },
    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      newdata = mlr3learners:::ordered_features(task, self)

      if (isTRUE(pv$se.method == "law_of_total_variance")) {
        prediction_nodes = mlr3misc::invoke(predict, self$model$model, data = newdata, type = "terminalNodes", .args = pv[setdiff(names(pv), "se.method")], predict.all = TRUE)
        observation_node_table = as.matrix(prediction_nodes$predictions)
        storage.mode(observation_node_table) = "integer"
        .Call("c_ltv_var", observation_node_table, self$model$mu_sigma2_per_node_per_tree)
      } else if (isTRUE(pv$se.method == "simple")) {
        prediction_nodes = mlr3misc::invoke(predict, self$model$model, data = newdata, type = "terminalNodes", .args = pv[setdiff(names(pv), "se.method")], predict.all = TRUE)
        observation_node_table = as.matrix(prediction_nodes$predictions)
        storage.mode(observation_node_table) = "integer"
        .Call("c_simple_var", observation_node_table, self$model$mu_sigma2_per_node_per_tree)
      } else {
        prediction = mlr3misc::invoke(predict, self$model$model, data = newdata, type = self$predict_type, .args = pv)
        list(response = prediction$predictions, se = prediction$se)
      }
    },

    .hotstart = function(task) {
      model = self$model
      model$num.trees = self$param_set$values$num.trees
      model
    }
  )
)

#' @export
default_values.LearnerRegrRangerMbo = function(x, search_space, task, ...) { # nolint
  special_defaults = list(
    mtry = floor(sqrt(length(task$feature_names))),
    mtry.ratio = floor(sqrt(length(task$feature_names))) / length(task$feature_names),
    sample.fraction = 1
  )
  defaults = insert_named(default_values(x$param_set), special_defaults)
  defaults[search_space$ids()]
}

#' @include aaa.R
learners[["regr.ranger_mbo"]] = LearnerRegrRangerMbo
