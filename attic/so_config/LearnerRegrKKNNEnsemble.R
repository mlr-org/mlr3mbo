LearnerRegrEnsemble = R6Class("LearnerRegrEnsemble",
  inherit = LearnerRegr,
  public = list(
    learners = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(learners) {
      self$learners = assert_learners(learners)  # i.e., kknn

      ps = ps(
      )

      super$initialize(
        id = "regr.ensemble",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        packages = "mlr3learners",
        man = ""
      )
    }
  ),

  private = list(
    .train = function(task) {
      for (i in seq_along(self$learners)) {
        self$learners[[i]]$predict_type = "response"
        self$learners[[i]]$train(task)
      }
      list()
    },

    .predict = function(task) {
      predictions = map_dtr(self$learners, function(learner) {
        as.data.table(learner$predict(task))
      })
      predictions = predictions[, .(mean = mean(response), se = sd(response)), by = .(row_ids)]
      list(response = predictions$response, se = predictions$se)
    }
  )
)
