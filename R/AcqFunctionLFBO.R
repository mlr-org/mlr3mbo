#' @title Acquisition Function LFBO
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_lfbo
#'
#' @templateVar id lfbo
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Likelihood free Bayesian Optimization.
#' Parameters specifying the weighting type and the gamma quantile of the target distribition have to be set via
#' the [paradox::ParamSet] of the [LearnerRegrLFBO] used within the [SurrogateLearner].
#'
#' @references
#' * `r format_bib("song_2022")`
#'
#' @family Acquisition Function
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("ranger")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'   library(data.table)
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
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = LearnerRegrLFBO$new(lrn("classif.ranger"))
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("lfbo", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionLFBO = R6Class("AcqFunctionLFBO",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)

      if (!is.null(surrogate)) {
        assert_r6(surrogate$model, "LearnerRegrLFBO", null.ok = TRUE)
        surrogate$model$surrogate_max_to_min = surrogate_mult_max_to_min(surrogate$archive$codomain, y_cols = surrogate$y_cols)
      }

      super$initialize("acq_lfbo", surrogate = surrogate, direction = "maximize", label = "Likelihood Free Bayesian Optimization", man = "mlr3mbo::mlr_acqfunctions_lfbo")
    }
  ),

  active = list(
    #' @field surrogate ([SurrogateLearner])\cr
    #'  Surrogate learner encapsulating a [LearnerRegrLFBO].
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        assert_r6(rhs, classes = "SurrogateLearner")
        assert_r6(surrogate$model, "LearnerRegrLFBO")
        private$.surrogate = rhs
        private$.archive = assert_r6(rhs$archive, classes = "Archive")
        codomain = generate_acq_codomain(rhs$archive$codomain, id = self$id, direction = self$direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(rhs$archive$codomain, y_cols = rhs$y_cols)
        domain = rhs$archive$search_space$clone(deep = TRUE)
        domain$trafo = NULL
        self$codomain = Codomain$new(codomain$params)  # lazy initialization requires this
        self$domain = domain

        private$.surrogate$model$surrogate_max_to_min = self$surrogate_max_to_min
      }
    },

    #' @field surrogate_max_to_min (`-1` | `1`)\cr
    #'   Multiplicative factor to correct for minimization or maximization of the acquisition function.
    #'   Changing this value will be propagated to the `$surrogate$model$surrogate_max_to_min` when possible.
    surrogate_max_to_min = function(rhs) {
     if (missing(rhs)) {
        private$.surrogate_max_to_min
      } else {
        private$.surrogate_max_to_min = assert_subset(rhs, choices = c(-1L, 1L))
        if (!is.null(private$.surrogate)) {
          private$.surrogate$model$surrogate_max_to_min = private$.surrogate_max_to_min
        }
      }
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      p = self$surrogate$predict(xdt)
      data.table(acq_lfbo = p$mean)
    }
  )
)

mlr_acqfunctions$add("lfbo", AcqFunctionLFBO)

