#' @title Acquisition Function LFBO
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_lfbo
#'
#' @templateVar id lfbo
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Likelihood-Free Bayesian Optimization.
#' Parameters specifying the weighting type and the gamma quantile of the target distribution have to be set in
#' the [paradox::ParamSet] of the [LearnerRegrLFBO] used within the [SurrogateLearner].
#'
#' By default, it is assumed that the optimization problem is a minimization problem!
#' If maximization is required, change the value of the direction parameter of the [LearnerRegrLFBO] used within the
#' [SurrogateLearner].
#'
#' @references
#' * `r format_bib("song_2022")`
#'
#' @family Acquisition Function
#' @export
#' @examples
#' \donttest{
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
#'   learner = lrn("regr.lfbo", learner_classif = lrn("classif.ranger"), lfbo.direction = "minimize")
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("lfbo", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
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
      super$initialize("acq_lfbo", surrogate = surrogate, requires_predict_type_se = FALSE, direction = "maximize", label = "Likelihood-Free Bayesian Optimization", man = "mlr3mbo::mlr_acqfunctions_lfbo")
    }
  ),

  private = list(
    .fun = function(xdt) {
      p = self$surrogate$predict(xdt)
      data.table(acq_lfbo = p$mean)
    }
  )
)

mlr_acqfunctions$add("lfbo", AcqFunctionLFBO)

