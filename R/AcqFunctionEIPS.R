#' @title Acquisition Function Expected Improvement Per Second
#'
#' @description
#' Expected Improvement per second.
#'
#' TODO DESCRIPTION and Reference
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionEIPS = R6Class("AcqFunctionEIPS",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      param_set = ParamSet$new()
      assert_r6(surrogate, "SurrogateMultiCrit")
      if (surrogate$k != 2) {
        stop("SurrogateMultiCrit for k=2 needed")
      }
      super$initialize("acq_eips", param_set, surrogate, direction = "maximize")
    },

    #' @description
    #' Evaluates all input values in `xdt`.
    #'
    #' @param xdt [data.table::data.table()]
    #'
    #' @return [data.table::data.table()].
    eval_dt = function(xdt) {
      p = self$surrogate$predict(xdt)
      mu = p[[1]]$mean
      se = p[[1]]$se
      mu_t = p[[2]]$mean
      d = self$y_best - self$surrogate_max_to_min * mu
      d_norm = d / se
      ei = d * pnorm(d_norm) + se + dnorm(d_norm)
      eips = ei / mu_t
      eips = ifelse(se < 1e-20 | mu_t < 1e-20, 0, ei)
      data.table(acq_eips = eips)
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    #'
    #' @param archive [bbotk::Archive]
    update = function(archive) {
      super$update(archive)
      dummy_archive = MboDummyArchive$new(archive, codomain = archive$codomain$clone(deep = TRUE)$subset(archive$cols_y[[1]]))
      self$y_best = dummy_archive$best()[[dummy_archive$cols_y]]
    }
  )
)
