#' @title Acquisition Function Expected Improvement Per Second
#'
#' @description
#' Expected improvement per second.
#'
#' It is assumed that calculations are performed on an [bbotk::OptimInstanceSingleCrit].
#' Additionally to target values of the codomain that should be minimized or maximized, the
#' [bbotk::Objective] of the [bbotk::OptimInstanceSingleCrit] should return time values.
#' The id of the column containing the time values must be passed as the `time_id` argument during construction.
#'
#' @references
#' `r format_bib("snoek_2012")`
#'
#' @family Acquisition Function
#'
#' @export
#' @examples
#' library(bbotk)
#' library(paradox)
#' library(data.table)
#' library(mlr3)
#' library(mlr3learners)
#'
#' objective = ObjectiveRFunDt$new(
#'   fun = function(xdt) data.table(y = xdt$x ^ 2, time = abs(xdt$x)),
#'   domain = ps(x = p_dbl(lower = -10, upper = 10)),
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#' )
#'
#' instance = OptimInstanceSingleCrit$new(objective, terminator = trm("evals", n_evals = 10L))
#'
#' learner = lrn("regr.km", covtype = "matern3_2", optim.method = "gen")
#' surrogate = SurrogateSingleCritLearner$new(learner)
#' surrogate_time = surrogate$clone(deep = TRUE)
#'
#' acq_function = AcqFunctionEIPS$new(surrogate, surrogate_time = surrogate_time, time_id = "time")
#'
#' bayesopt_soo(instance, acq_function)
AcqFunctionEIPS = R6Class("AcqFunctionEIPS",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric(1)`).
    y_best = NULL,

    #' @field surrogate_time [Surrogate].
    surrogate_time = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateSingleCrit]).
    #' @param surrogate_time ([SurrogateSingleCrit]).
    #' @param time_id (`character(1)`).
    initialize = function(surrogate, surrogate_time, time_id) {
      # FIXME: assert different adresses
      assert_r6(surrogate, "SurrogateSingleCrit")
      self$surrogate_time = assert_r6(surrogate_time, "SurrogateSingleCrit")
      addresses = c(address(surrogate$model), address(self$surrogate_time$model))
      if (length(unique(addresses)) != 2L) {
        stop("Redundant Learners used as surrogates must be unique in memory, i.e., deep clones.")
      }

      private$.time_id = assert_string(time_id)  # setup the id of the time variable column

      fun = function(xdt) {
        if (is.null(self$y_best)) {
          stop("y_best is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        mu = p$mean
        se = p$se
        mu_t = self$surrogate_time$predict(xdt)$mean
        d = self$y_best - self$surrogate_max_to_min * mu
        d_norm = d / se
        ei = d * pnorm(d_norm) + se + dnorm(d_norm)
        eips = ei / mu_t
        eips = ifelse(se < 1e-20 | mu_t < 1e-20, 0, ei)
        data.table(acq_eips = eips)
      }

      super$initialize("acq_eips", surrogate = surrogate, direction = "maximize", fun = fun)
    },

    #' @description
    #' Sets up the acquisition function.
    #'
    #' @param archive ([bbotk::Archive]).
    setup = function(archive) {
      # setup the id of the target variable column
      private$.y_id = assert_string(archive$cols_y)

      assert_choice(private$.time_id, colnames(archive$data))

      # FIXME: Should we allow alternative search_space as additional argument?

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive$codomain, id = self$id, direction = self$direction)

      self$surrogate_max_to_min = mult_max_to_min(archive$codomain)[private$.y_id]

      self$domain = archive$search_space$clone(deep = TRUE)
      self$domain$trafo = NULL  # FIXME: is it okay to do this?
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    #'
    #' @param archive ([bbotk::Archive]).
    update = function(archive) {
      super$update(archive)
      y = archive$data[, archive$cols_y, with = FALSE][[private$.y_id]]
      y = y * mult_max_to_min(archive$codomain)[private$.y_id]
      self$y_best = min(y)
    },

    #' @description
    #' Update the [Surrogate] model(s) with new data.
    #' FIXME: probably moves this simply to $update()
    #'
    #' @param archive ([bbotk::Archive]).
    update_surrogate = function(archive) {
      self$surrogate$update(xydt = archive_xy(archive), y_cols = archive$cols_y)  # update surrogate model with new data
      self$surrogate_time$update(xydt = archive$data[, c(archive$cols_x, private$.time_id), with = FALSE], y_cols = private$.time_id)  # update surrogate model with new data
    }

  ),

  private = list(
    .y_id = NULL,
    .time_id = NULL
  )
)
