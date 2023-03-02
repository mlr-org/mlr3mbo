#' @title Gower Distance Nearest Neighbor Regression Ensemble
#'
#' @name mlr_learners_regr.gower_nn_ensemble
#'
#' @description
#' FIXME:
#'
#' @templateVar id regr.gower_nn_ensemble
#' @template learner
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrGowerNNEnsemble = R6Class("LearnerRegrGowerNNEnsemble",
  inherit = LearnerRegr,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        ks = p_uty(tags = "train"),
        sample.fraction = p_dbl(lower = 0, upper = 1, tags = "train"),
        replace = p_lgl(tags = "train"),
        mtry.ratio = p_dbl(lower = 0, upper = 1, tags = "train"),
        nthread = p_int(lower = 1, tags = "predict")
      )

      ps$values$ks = c(1L, 3L, 5L, 7L, 10L)
      ps$values$sample.fraction = 1
      ps$values$replace = TRUE
      ps$values$nthread = 1L

      super$initialize(
        id = "regr.gower_nn_ensemble",
        param_set = ps,
        predict_types = c("response", "se"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = character(0),
        packages = c("mlr3mbo", "gower"),
        man = "mlr3mbo::mlr_learners_regr.gower_nn_ensemble"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if(is.null(pv$mtry.ratio)) {
        pv = insert_named(pv, list(mtry.ratio = floor(sqrt(length(task$feature_names))) / length(task$feature_names)))
      }
      if (any(pv$ks >= task$nrow)) {
        index = which(pv$ks >= task$nrow)
        stopf("Parameter k = %i must be smaller than the number of observations n = %i", pv$k[index], task$nrow)
      }

      list(
        target_name = task$target_names,
        feature_names = task$feature_names,
        data = task$data(),
        pv = pv
      )

    },

    .predict = function(task) {
      model = self$model
      newdata = mlr3learners:::ordered_features(task, self)
      pv = insert_named(model$pv, self$param_set$get_values(tags = "predict"))

      data = self$model$data
      X = data[, self$model$feature_names, with = FALSE]
      y = data[, self$model$target_name, with = FALSE]

      stopifnot(all(colnames(X) == colnames(newdata)))

      mtry = as.integer(round(pv$mtry.ratio * ncol(X)))

      # FIXME: what about ordered here?
      for (feature in colnames(X)) {
        if ("factor" %in% class(X[[feature]])) {
          factor_levels = union(levels(X[[feature]]), levels(newdata[[feature]]))
        levels(X[[feature]]) = factor_levels
        levels(newdata[[feature]]) = factor_levels
        }
      }

      n = as.integer(max(ceiling(pv$sample.fraction * nrow(X)), nrow(X)))

      results = map_dtr(pv$ks, function(k) {
        ids = sample(seq_len(nrow(X)), size = n, replace = pv$replace)
        features = sample(colnames(X), size = mtry, replace = FALSE)
        top_n = invoke(gower::gower_topn, x = newdata[, features, with = FALSE], y = X[ids, features, with = FALSE], n = k, nthread = pv$nthread)$index
        tmp = map_dtc(seq_len(k), function(i) {
          setNames(y[ids, ][top_n[i, ], ], nm = paste0(self$model$target_name, "_", i))
        })
        mu = rowMeans(tmp)
        sigma_2 = apply(tmp, MARGIN = 1L, FUN = var)
        sigma_2[is.na(sigma_2)] = 0
        result = data.table(mu = mu, sigma_2 = sigma_2)
        result[, k := k]
        result[, ID := seq_len(.N)]
        result
      })

      response = results[, .(response = mean(mu)), by = .(ID)]
      results[, first_term := 0.001 + sigma_2 + mu ^ 2]
      variance = results[, .(variance = mean(first_term)), by = .(ID)]
      setorderv(response, col = "ID")
      setorderv(variance, col = "ID")
      stopifnot(all(response[["ID"]] == variance[["ID"]]))
      variance$variance = variance$variance - response$response ^ 2

      list(response = response$response, se = sqrt(variance$variance))
    }
  )
)

