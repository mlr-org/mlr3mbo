#' @title Likelihood Free Bayesian Optimization Learner
#'
#' @description
#' Wraps a classification learner to be used as a regression learner for
#' Likelihood-free Optimization.
#'
#' @details
#' The regression data is internally converted to a weighted classification task.
#' The new objective is a weighted version of a prediction problem, where the goal
#' to predict whether an instance is smaller than the `gamma` quantile of the target
#' distribution.
#' 
#' @references
#' * `r format_bib("song_2022")`
#' 
#' @export
LearnerRegrLFBO = R6Class("LearnerRegrLFBO",
  inherit = mlr3::LearnerRegr,
  public = list(

    #' @field clf ([mlr3::LearnerClassif])\cr
    #'   Classifier to be used for LFBO.
    clf = NULL,
    #' @field wt (character)\cr
    #'   Weighting scheme for LFBO. Currently allows "ei" and "pi".
    wt = NULL,
    #' @field gamma (numeric)\cr
    #'   Quantile of the target distribution. Defaults to `0.33`.
    gamma = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param clf ([mlr3::LearnerClassif])\cr
    #'   Classifier to be used for LFBO. Needs to have predict_type "prob".
    #' @param wt (character)\cr
    #'   Weighting scheme for LFBO. Currently allows "ei" (expected improvement) and "pi" (probability of improvement).
    #' @param gamma (numeric)\cr
    #'   Quantile of the target distribution. Defaults to `0.33`.
    initialize = function(clf, gamma = 0.33, wt = "ei") {
      assert_class(clf, "LearnerClassif")
      assert_true(clf$predict_type == "prob")
      assert_true("weights" %in% clf$properties)
      # FIXME: Should we expose gamma, wt as params, and if: how without making things ugly.
      self$clf = clf
      self$gamma = assert_number(gamma, lower = 0, upper = 1)
      self$wt = assert_choice(wt, choices = c("ei", "pi"))
      props = setdiff(clf$properties, c("twoclass", "multiclass"))
      super$initialize(
        id = paste0(clf$id, "2regr"),
        param_set = clf$param_set, feature_types = clf$feature_types,
        predict_types = c("response"),
        properties = props, data_formats = clf$data_formats, packages = clf$packages,
        label = clf$label, man = clf$man
      )
    },
    train = function(task, row_ids = NULL) {
        # create df with new data.
        tn = private$.to_weighted_task(task, row_ids)
        self$clf$train(tn, row_ids)
        self$state$train_task = mlr3:::task_rm_backend(task$clone(deep = TRUE))
        invisible(NULL)
    },
    predict = function(task, row_ids = NULL) {
      fts = task$data(rows = row_ids, cols = task$feature_names)
      colnames(fts) = paste0("X.", colnames(fts))
      pd = self$clf$predict_newdata(fts)$data
      pdata = as_prediction_data(
        list(response = - pd$prob[, 2L]),
        task = task,
        row_ids = pd$row_ids
      )
      as_prediction(pdata)
    }
  ),
  private = list(
    .to_weighted_task = function(task, row_ids) {
      dt = do.call("cbind", private$.regr_to_weights(
        task$data(rows = row_ids, cols = task$feature_names),
        task$data(rows = row_ids, cols = task$target_names)[[1]]
      ))
      tn = TaskClassif$new("weighted_classif", dt, target = "z")
      tn$set_col_roles("w", "weight")
    },
    .regr_to_weights = function(X,y, gamma = .8, wt = "ei") {
      # Adapted from https://github.com/lfbo-ml/lfbo
      tau = quantile(y, probs = self$gamma)
      z = y < tau
      if (self$wt == "ei") {
        w1 = tau - y[z]
        w1 = w1 / mean(w1)
        Xn = rbind(X, X[z,])
        zn = c(rep(0, length(z)), z[z])
        wn = c(rep(1, length(z)) / length(z), w1 * sum(z))
        wn = wn / mean(wn)
      } else if (self$wt == "pi") {
        Xn = X
        wn = rep(1, nrow(X))
        zn = z
      }
      return(list(X=Xn,w=wn,z=as.factor(zn)))
    }
  )
)

mlr_learners$add("regr.lfbo", function() LearnerRegrLFBO$new())
