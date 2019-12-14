Surrogate = R6Class("Surrogate",

  public = list(
    learner = NULL,

    initialize = function(learner) {
      # FIXME: assert for regr
      self$learner = assert_learner(learner)
    },

    train = function(archive) {
      task = TaskRegr$new(backend = archive$data, target = "y", id = "surrogate_task")
      self$learner$train(task)
    },
    
    predict_newdata = function(newdata) {
      self$learner$predict_newdata(newdata) 
    }
  )
)

