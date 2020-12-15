#' @title OptimizerMbo
#'
#' @name mlr_optimizers_mbo
#'
#' @description
#' MBO loop as Optimizer.
#'
#' @export
OptimizerMbo = R6Class("OptimizerMbo",
  inherit = bbotk::Optimizer,

  public = list(

    #' @field loop_function (`function`).
    loop_function = NULL, #FIXME: At this point it DOES look ok to have the different mbo algos as objects, right?

    #' @field result_function (`function`).
    result_function = NULL,

    #' @field acq_function ([AcqFunction]).
    acq_function = NULL,

    #' @field acq_optimizer ([AcqOptimizer]).
    acq_optimizer = NULL,

    #' @field args (`list()`).
    args = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param loop_function (`function`).
    #' @param acq_function ([AcqFunction]).
    #' @param acq_optimizer ([AcqOptimizer]).
    #' @param args (`list()`).
    #' @param result_function (`function`).
    initialize = function(loop_function, acq_function, acq_optimizer, args = NULL, result_function = NULL) {
      param_set = ParamSet$new()
      param_classes = feature_types_to_param_classes(acq_function$surrogate$model$feature_types)
      properties = c("single-crit", "dependencies") # FIXME: Should depend on the settings?
      packages = character() # Maybe not so important? Surrogate package etc?
      super$initialize(param_set, param_classes, properties, packages)
      self$loop_function = assert_function(loop_function)
      self$acq_function = assert_r6(acq_function, "AcqFunction")
      self$acq_optimizer = assert_r6(acq_optimizer, "AcqOptimizer")
      self$args = assert_list(args, names = "named", null.ok = TRUE)
      self$result_function = assert_function(result_function, null.ok = TRUE)
    }
  ),

  private = list(
    .optimize = function(inst) {
      do.call(self$loop_function, c(list(instance = inst, acq_function = self$acq_function, acq_optimizer = self$acq_optimizer), self$args))
    },

    .assign_result = function(inst) {
      if (is.null(self$result_function)) {
        super$.assign_result(inst)
      } else {
        #FIXME: Where do we define how to assign the result? i.e. if the problem is stochastic we dont want to chose the best point but the point with the best mean prediction
        self$result_function(inst, self) #FIXME: Maybe not final API
      }
    }
  )
)
