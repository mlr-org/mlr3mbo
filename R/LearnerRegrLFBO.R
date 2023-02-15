#' @title Likelihood Free Bayesian Optimization Learner
#'
#' @name mlr_learners_regr_lfbo
#'
#' @description
#' Wraps a classification learner to be used as a regression learner for
#' Likelihood-free Bayesian Optimization (LFBO).
#'
#' @details
#' The regression data is internally converted to a weighted classification task.
#' The new objective is a weighted version of a prediction problem, where the goal is
#' to predict whether a target value is smaller than a gamma quantile of the target
#' distribution (assuming minimization).
#'
#' To specify the weighting type, set the value of the `lfbo.wt` parameter (`"ei"` or `"pi"`).
#' To specify the `gamma` quantile of the target distribution set the value of the `lfbo.gamma` parameter.
#'
#' @references
#' * `r format_bib("song_2022")`
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("ranger")) {
#'
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
#'   instance = OptimInstanceSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   surrogate = srlrn(lrn("regr.lfbo", lrn("classif.ranger")))
#'
#'   acq_function = acqf("lfbo")
#'
#'   acq_optimizer = acqo(
#'     optimizer = opt("random_search", batch_size = 100),
#'     terminator = trm("evals", n_evals = 100))
#'
#'   optimizer = opt("mbo",
#'     loop_function = bayesopt_ego,
#'     surrogate = surrogate,
#'     acq_function = acq_function,
#'     acq_optimizer = acq_optimizer)
#'
#'   optimizer$optimize(instance)
#' }
#' }
LearnerRegrLFBO = R6Class("LearnerRegrLFBO",
  inherit = mlr3::LearnerRegr,
  public = list(
    #' @field learner_classif ([mlr3::LearnerClassif])\cr
    #'   Classification learner to be used for LFBO.
    learner_classif = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner_classif ([mlr3::LearnerClassif])\cr
    #'   Classifcation learner to be used for LFBO.
    #'   Requires `predict_type = "prob"` which will be set automatically during construction.
    #'   Also requires the learner to be able to handle case weights, see `learner$properties`.
    initialize = function(learner_classif) {
      assert_class(learner_classif, "LearnerClassif")
      assert_true("prob" %in% learner_classif$predict_types)
      learner_classif$predict_type = "prob"
      assert_true("weights" %in% learner_classif$properties)
      self$learner_classif = learner_classif

      lfbo_param_set = ps(wt = p_fct(levels = c("ei", "pi"), default = "ei", tags = "train"), gamma = p_dbl(lower = 0, upper = 1, default = 1/3, tags = "train"))
      lfbo_param_set$set_id = "lfbo"

      super$initialize(
        id = "regr.lfbo",
        param_set = ParamSetCollection$new(list(lfbo_param_set, self$learner_classif$param_set)),
        feature_types = learner_classif$feature_types,
        predict_types = "response",
        properties = setdiff(learner_classif$properties, c("twoclass", "multiclass")),
        data_formats = learner_classif$data_formats,
        packages = learner_classif$packages,
        label = learner_classif$label,
        man = "mlr3mbo::mlr_learners_regr_lfbo"
      )
    },

    #' @description
    #' Train the learner on a set of observations of the provided `task`.
    #' Mutates the learner by reference, i.e. stores the model alongside other information in field `$state`.
    #'
    #' @param task ([TaskRegr]).
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of training indices as subset of `task$row_ids`.
    #'   For a simple split into training and test set, see [mlr3::partition()].
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    train = function(task, row_ids = NULL) {
      # create df with new data
      if (is.null(self$surrogate_max_to_min)) {
        stop("$surrogate_max_to_min must be set prior to training.")
      }
      row_ids = assert_row_ids(row_ids, null.ok = TRUE)
      task_new = private$.to_weighted_task(task, row_ids)
      self$learner_classif$train(task_new, row_ids)
      self$state$train_task = task_rm_backend(task$clone(deep = TRUE))
      invisible(NULL)
    },

    #' @description
    #' Uses the information stored during `$train()` in `$learner$classif$state` to create a new [mlr3::PredictionRegr]
    #' for a set of observations of the provided `task`.
    #'
    #' @param task ([mlr3::TaskRegr]).
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of test indices as subset of `task$row_ids`.
    #'   For a simple split into training and test set, see [mlr3::partition()].
    #'
    #' @return [PredictionRegr].
    predict = function(task, row_ids = NULL) {
      row_ids = assert_row_ids(row_ids, null.ok = TRUE)
      features = task$data(rows = row_ids, cols = task$feature_names)
      colnames(features) = paste0("X.", colnames(features))
      pred = self$learner_classif$predict_newdata(features)$data
      prediction_data = as_prediction_data(list(response = pred$prob[, 2L]), task = task, row_ids = pred$row_ids)
      as_prediction(prediction_data)
    }
  ),

  active = list(
    #' @field surrogate_max_to_min (`-1` | `1`)\cr
    #'   Multiplicative factor to correct for minimization or maximization.
    surrogate_max_to_min = function(rhs) {
     if (missing(rhs)) {
        private$.surrogate_max_to_min
      } else {
        private$.surrogate_max_to_min = assert_subset(rhs, choices = c(-1L, 1L))
      }
    }
  ),

  private = list(
    .surrogate_max_to_min = NULL,

    .to_weighted_task = function(task, row_ids) {
      data = do.call("cbind", private$.regr_to_weights(
        task$data(rows = row_ids, cols = task$feature_names),
        task$data(rows = row_ids, cols = task$target_names)[[1]]
      ))
      task_new = TaskClassif$new("weighted_classif", data, target = "z")
      task_new$set_col_roles("w", "weight")
    },

    .regr_to_weights = function(X, y) {
      # wt and gamma are inferred from the param_set
      # surrogate_max_to_min is inferred from the active bindings
      # adapted from https://github.com/lfbo-ml/lfbo
      wt = if (is.null(self$param_set$values$lfbo.wt)) "ei" else self$param_set$values$lfbo.wt
      gamma = if (is.null(self$param_set$values$lfbo.gamma)) 1/3 else self$param_set$values$lfbo.gamma
      y = self$surrogate_max_to_min * y
      tau = quantile(y, probs = gamma)
      z = y < tau
      if (wt == "ei") {
        w1 = tau - y[z]
        w1 = w1 / mean(w1)
        Xn = rbind(X, X[z,])
        zn = c(rep(0, length(z)), z[z])
        wn = c(rep(1, length(z)) / length(z), w1 * sum(z))
        wn = wn / mean(wn)
      } else if (wt == "pi") {
        Xn = X
        wn = rep(1, nrow(X))
        zn = z
      }
      return(list(X = Xn, w = wn, z = as.factor(zn)))
    },

    .train = function(task) {},

    .predict = function(task) {}
  )
)

