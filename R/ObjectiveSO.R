ObjectiveSO = R6Class("ObjectiveSO", inherit = Objective,

  public = list(

    initialize = function(fun, param_set, terminator, minimize = TRUE) {
      super$initialize(fun, param_set, terminator, minimize)
    },

    best = function(measure = NULL) {
      # FIXME: this uses mlr concepts, like resampling
      # FIXME: and should this really live here? or in archive?
      if (is.null(measure)) {
        measure = self$measures[[1L]]
      } else {
        measure = as_measure(measure, task_type = self$task$task_type)
        # check that we are only using contained measures
        assert_choice(measure$id, map_chr(self$measures, "id"))
      }
      assert_measure(measure, task = self$task, learner = self$learner)
      if (is.na(measure$minimize))
        stopf("Measure '%s' has minimize = NA and hence cannot be tuned", measure$id)

      tab = self$bmr$aggregate(measure, ids = FALSE)
      y = tab[[measure$id]]
      if (allMissing(y))
        stopf("No non-missing performance value stored")

      which_best = if (measure$minimize) which_min else which_max
      best_index = which_best(y, na_rm = TRUE)
      tab$resample_result[[best_index]]
    }
  )
)

