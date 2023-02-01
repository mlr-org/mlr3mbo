LearnerR2C = R6Class("LearnerR2C",
  inherit = mlr3::LearnerRegr,
  public = list(
    #' classifier
    classifier = NULL,
    #' wt
    wt = NULL,
    #' gamma
    gamma = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(learner, gamma = 0.9, wt = "ei") {
      assert_class(learner, "LearnerClassif")
      assert_true(learner$predict_type == "prob")
      assert_true("weights" %in% learner$properties)
      l = learner
      self$classifier = learner
      self$gamma = assert_number(gamma, lower = 0, upper = 1)
      self$wt = assert_choice(wt, choices = c("ei", "pi"))
      props = setdiff(l$properties, c("twoclass", "multiclass"))
      super$initialize(
        id = paste0(l$id, "2regr"),
        param_set = l$param_set, feature_types = l$feature_types,
        predict_types = c("response", "se"),
        properties = props, data_formats = l$data_formats, packages = l$packages,
        label = l$label, man = l$man)
    },
    train = function(task, row_ids = NULL) {
        # create df with new data.
        tn = private$.to_weighted_task(task, row_ids)
        self$classifier$train(tn, row_ids)
        self$state$train_task = mlr3:::task_rm_backend(task$clone(deep = TRUE))
    },
    predict = function(task, row_ids = NULL) {
      fts = task$data(rows = row_ids, cols = task$feature_names)
      colnames(fts) = paste0("X.", colnames(fts))
      pd = self$classifier$predict_newdata(fts)$data
      pdata = as_prediction_data(
        list(response = pd$prob[, 1L]),
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
    .regr_to_weights = function(X,y, gamma = .9, wt = "ei") {
        tau = quantile(y,  p=self$gamma)
        z = y < tau

        if (self$wt == "ei") {
            w1 = tau - y[z]
            w1 = w1 / mean(w1)
            Xn = rbind(X, X[z,])
            zn = c(z[z], rep(0, length(z)))
            wn = c(w1 * sum(z), rep(1, length(z)) / length(z))
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

library(mlr3)
library(data.table)
library(checkmate)
library(R6)
library(mlr3mbo)
library(bbotk)
library(paradox)
library(mlr3learners)
fun = function(xs) {
  list(y = xs$x ^ 2)
}
domain = ps(x = p_dbl(lower = -10, upper = 10))
codomain = ps(y = p_dbl(tags = "minimize"))
objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 20)
)
surrogate = SurrogateLearner$new(LearnerR2C$new(lrn("classif.rpart", predict_type = "prob")))
acq_function = acqf("mean")
acq_optimizer = acqo(
  optimizer = opt("random_search"),
  terminator = trm("evals", n_evals = 100))
optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer
)
optimizer$optimize(instance)

dt = instance$archive
ggplot(dt$data) + geom_point(aes(x = x, y = acq_mean), size = 5) + theme_minimal()