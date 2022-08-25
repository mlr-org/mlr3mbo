#' @title Custom Ranger Regression Learner
#'
#' @name mlr_learners_regr.ranger_custom
#'
#' @description
#' Custom random regression forest.
#' Calls [ranger::ranger()] from package \CRANpkg{ranger}.
#' This custom random regression forest tries to mimic the behaviour of a Gaussian process and therefore may be more suitable for Bayesian optimization compared to a normal random regression forest.
#'
#' The `"extratrees"` splitting rule is used with `num.random.splits = 1`.
#' This results in a rather smooth prediction function.
#' No bootstrap or subsampling is used but rather all training data (`replace = FALSE` and `sample.fraction = 1`).
#' This may seem counter-intuitive at first but ensures that training data is almost perfectly interpolated (given `min.node.size = 1`).
#' Per default, `1000` trees are used.
#' Classical standard error estimation is no longer possible, but the `"extratrees"` splitting rule allows for estimating the variance based on the variance of the trees (simply taking the variance over the predictions of the trees).
#' This results in low variance close to training data points.
#' If `se.simple.spatial = TRUE` this point-wise variance is upwardly biased by adding the pointwise variance again scaled by the minimum Gower distance of the point to the training data (recall that a Gower distance of 0 means an identical point is present in the training data, whereas a distance of 1 means total dissimilarity to the training data).
#' This ensures that the variance is larger in regions dissimilar to the training data.
#' Summarizing, the standard error for a point is calculated as follows:
#' \deqn{\hat{\mathrm{SE}}(x_{i}) = \sqrt{(\hat{\mathrm{VAR}}(x_{i}) + \epsilon) + (\hat{\mathrm{VAR}}(x_{i}) + \epsilon) * \mathrm{GD}_{i})}}
#' Here, \eqn{\epsilon} is given by `se.simple.spatial.nugget` which can be set larger than 0 in the case of noisy function observations.
#'
#' @templateVar id regr.ranger_custom
#' @template learner
#'
#' @export
#' @template seealso_learner
#' @examples
#' library(data.table)
#' library(mlr3)
#' x = seq(-5, 5, length.out = 1001)
#' dat = data.table(x = c(-5, -2.5, -1, 0, 1, 2.5, 5), y = c(0, -0.1, 0.3, -1, 0.3, -0.1, 0))
#' task = TaskRegr$new("example", backend = dat, target = "y")
#' learner = lrn("regr.ranger_custom")
#' learner$predict_type = "se"
#' learner$train(task)
#' predictions = learner$predict_newdata(data.table(x = x))
#' if (requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'   ggplot(aes(x = x, y = response), data = cbind(x, as.data.table(predictions))) +
#'     geom_line() +
#'     geom_ribbon(aes(min = response - se, max = response + se), alpha = 0.1) +
#'     geom_point(aes(x = x, y = y), data = task$data()) +
#'     labs(x = "x", y = "Prediction") +
#'     theme_minimal()
#' }
LearnerRegrRangerCustom = R6Class("LearnerRegrRangerCustom",
  inherit = mlr3::LearnerRegr,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        always.split.variables       = p_uty(tags = "train"),
        holdout                      = p_lgl(default = FALSE, tags = "train"),
        importance                   = p_fct(c("none", "impurity", "impurity_corrected", "permutation"), tags = "train"),
        keep.inbag                   = p_lgl(default = FALSE, tags = "train"),
        local.importance             = p_lgl(default = FALSE, tags = "train"),
        max.depth                    = p_int(default = NULL, lower = 0L, special_vals = list(NULL), tags = "train"),
        mtry                         = p_int(lower = 1L, special_vals = list(NULL), tags = "train"),
        mtry.ratio                   = p_dbl(lower = 0, upper = 1, tags = "train"),
        num.threads                  = p_int(1L, default = 1L, tags = c("train", "predict", "threads")),  # changed
        num.trees                    = p_int(1L, default = 1000L, tags = c("train", "predict", "hotstart")),  # changed
        oob.error                    = p_lgl(default = FALSE, tags = "train"),  # changed
        regularization.factor        = p_uty(default = 1, tags = "train"),
        regularization.usedepth      = p_lgl(default = FALSE, tags = "train"),
        respect.unordered.factors    = p_fct(c("ignore", "order", "partition"), default = "ignore", tags = "train"),
        save.memory                  = p_lgl(default = FALSE, tags = "train"),
        scale.permutation.importance = p_lgl(default = FALSE, tags = "train"),
        se.simple.spatial            = p_lgl(default = TRUE, tags = "predict"),
        se.simple.spatial.nugget     = p_dbl(0, default = 0, tags = "predict"),
        seed                         = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "predict")),
        split.select.weights         = p_uty(default = NULL, tags = "train"),
        verbose                      = p_lgl(default = TRUE, tags = c("train", "predict")),
        write.forest                 = p_lgl(default = TRUE, tags = "train")
      )
      ps$values = list(num.trees = 1000L, oob.error = FALSE, num.threads = 1L)

      # deps
      ps$add_dep("se.simple.spatial.nugget", "se.simple.spatial", CondEqual$new(TRUE))

      super$initialize(
        id = "regr.ranger_custom",
        param_set = ps,
        predict_types = c("response", "se"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "importance", "oob_error", "hotstart_backward"),
        packages = c("mlr3mbo", "ranger"),
        man = "mlr3learners::mlr_learners_regr.ranger_custom"
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
      pv = convert_ratio_ranger(pv, "mtry", "mtry.ratio", length(task$feature_names))
      pv = insert_named(list(min.node.size = 1L, replace = FALSE, sample.fraction = 1L, splitrule = "extratrees", num.random.splits = 1L), pv)

      private$.train_task = task$clone(deep = TRUE)  # required for se.method == "simple" and se.simple.spatial == TRUE

      invoke(ranger::ranger, dependent.variable.name = task$target_names, data = task$data(), case.weights = task$weights$weight, .args = pv)
    },

    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      pv = insert_named(list(se.simple.spatial = TRUE, se.simple.spatial.nugget = 0), pv)
      newdata = ordered_features(task, self)

      prediction = invoke(predict, self$model, data = newdata, type = "response", predict.all = TRUE, .args = pv)
      response = apply(prediction$predictions, MARGIN = 1L, FUN = mean)
      variance = apply(prediction$predictions, MARGIN = 1L, FUN = var)
      if (pv$se.simple.spatial) {
        gw_dists = get_gower_dist(fct_to_char(newdata), fct_to_char(ordered_features(private$.train_task, self)))  # 0 if identical, 1 if maximally dissimilar
        min_gw_dists = apply(gw_dists, MARGIN = 1L, FUN = min)  # get the minium for each new point to the points used for training
        #min_gw_dists = (min_gw_dists - min(min_gw_dists)) / (max(min_gw_dists) - min(min_gw_dists))  # scale to [0, 1]
        #min_gw_dists[is.na(min_gw_dists)] = 0
        # upwardly bias variance by the mean variance scaled by the gower distance (and the potential nugget, useful for noisy)
        #variance = variance + (mean(variance) * (min_gw_dists + (pv$se.simple.spatial.nugget %??% 0)))  # 0 is default
        variance = (variance + pv$se.simple.spatial.nugget) + (variance + pv$se.simple.spatial.nugget) * min_gw_dists
      }
      se = sqrt(variance)
      list(response = response, se = if (self$predict_type == "se") se else NULL)
    },

    .hotstart = function(task) {
      model = self$model
      model$num.trees = self$param_set$values$num.trees
      model
    },

    .train_task = NULL
  )
)

#' @export
default_values.LearnerRegrRangerCustom = function(x, search_space, task, ...) { # nolint
  special_defaults = list(
    mtry = floor(sqrt(length(task$feature_names))),
    mtry.ratio = floor(sqrt(length(task$feature_names))) / length(task$feature_names),
    num.trees = 1000L,
    min.node.size = 1L,
    replace = FALSE,
    sample.fraction = 1,
    splitrule = "extratrees",
    num.random.splits = 1L
  )
  defaults = insert_named(default_values(x$param_set), special_defaults)
  defaults[search_space$ids()]
}

convert_ratio_ranger = function(pv, target, ratio, n) {
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

ordered_features = function(task, learner) {
  cols = names(learner$state$data_prototype)
  task$data(cols = intersect(cols, task$feature_names))
}

