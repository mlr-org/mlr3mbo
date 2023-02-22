#' @title Acquisition Function EHVI
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ehvi
#'
#' @description
#' Expected Hypervolume Improvement.
#'
#' @section Parameters:
#'
#' @references
#' * `r format_bib("emmerich_2016")`
#'
#' @family Acquisition Function
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'   library(data.table)
#'
#'   fun = function(xs) {
#'     list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceMultiCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = lrn("regr.km",
#'     covtype = "matern3_2",
#'     optim.method = "gen",
#'     nugget.stability = 10^-8,
#'     control = list(trace = FALSE))
#'
#'   surrogate = srlrnc(list(learner, learner$clone(deep = TRUE)), archive = instance$archive)
#'
#'   acq_function = acqf("ehvi", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionEHVI = R6Class("AcqFunctionEHVI",
  inherit = AcqFunction,

  public = list(

    #' @field ys_front (`matrix()`)\cr
    #'   Approximated Pareto front. Sorted by the first objective.
    #'   Signs are corrected for assuming minimization of objectives.
    ys_front = NULL,

    #' @field ref_point (`numeric()`)\cr
    #'   Reference point.
    ref_point = NULL,

    #' @field ys_front_augmented (`matrix()`)\cr
    #'   Augmented approximated Pareto front. Sorted by the first objective.
    #'   Signs are corrected for assuming minimization of objectives.
    ys_front_augmented = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearnerCollection]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearnerCollection", null.ok = TRUE)
      super$initialize("acq_ehvi", surrogate = surrogate, direction = "maximize", label = "EHVI", man = "mlr3mbo::mlr_acqfunctions_ehvi")
    },

    #' @description
    #' Updates acquisition function and sets `ys_front`, `ref_point`.
    update = function() {
      # FIXME: how do we handle different implementations for different ks
      #        the following works for k = 2
      n_obj = length(self$archive$cols_y)
      columns = self$archive$cols_y
      ys = self$archive$data[, self$archive$cols_y, with = FALSE]
      for (column in columns) {
        set(ys, j = column, value = ys[[column]] * self$surrogate_max_to_min[[column]])  # assume minimization
      }
      ys = as.matrix(ys)

      self$ref_point = apply(ys, MARGIN = 2L, FUN = min) - 1  # offset = 1 like in mlrMBO

      self$ys_front = self$archive$best()[, self$archive$cols_y, with = FALSE]
      for (column in columns) {
        set(self$ys_front, j = column, value = self$ys_front[[column]] * self$surrogate_max_to_min[[column]])  # assume minimization
      }
      setorderv(self$ys_front, cols = columns[1L], order = -1L)

      ys_front_augmented = rbind(t(setNames(c(self$ref_point[1L], - Inf), nm = columns)), self$ys_front, t(setNames(c(- Inf, self$ref_point[2L]), nm = columns)))

      self$ys_front = as.matrix(self$ys_front)

      self$ys_front_augmented = as.matrix(ys_front_augmented)
    }
  ),

  private = list(
    .fun = function(xdt) {
      if (is.null(self$ys_front)) {
        stop("$ys_front is not set. Missed to call $update()?")
      }
      if (is.null(self$ref_point)) {
        stop("$ref_point is not set. Missed to call $update()?")
      }
      if (is.null(self$ys_front_augmented)) {
        stop("$ys_front_augmented is not set. Missed to call $update()?")
      }

      columns = colnames(self$ys_front_augmented)
      
      ps = self$surrogate$predict(xdt)
      means = map_dtc(ps, "mean")

      for (column in columns) {
        set(means, j = column, value = means[[column]] * self$surrogate_max_to_min[[column]])
      }
      ses = map_dtc(ps, "se")

      map_dtr(seq_len(nrow(means)), function(j) {  # emmerich_2016 5.2.1 (16)
        first_summands = map_dbl(seq_len(nrow(self$ys_front_augmented))[-1L], function(i) {
          (self$ys_front_augmented[i - 1L, ][[columns[1L]]] - self$ys_front_augmented[i, ][[columns[1L]]]) *
            pnorm(self$ys_front_augmented[i, ][[columns[1L]]], mean = means[j, ][[columns[1L]]], sd = ses[j, ][[columns[1L]]]) *
            psi_function(a = self$ys_front_augmented[i, ][[columns[2L]]], b = self$ys_front_augmented[i, ][[columns[2L]]], mu = means[j, ][[columns[2L]]], sigma = ses[j, ][[columns[2L]]])
        })
        second_summands = map_dbl(seq_len(nrow(self$ys_front_augmented))[-1L], function(i) {
          (psi_function(a = self$ys_front_augmented[i - 1L, ][[columns[1L]]], b = self$ys_front_augmented[i - 1L, ][[columns[1L]]], mu = means[j, ][[columns[1L]]], sigma = ses[j, ][[columns[1L]]]) -
            psi_function(a = self$ys_front_augmented[i - 1L, ][[columns[1L]]], b = self$ys_front_augmented[i, ][[columns[1L]]], mu = means[j, ][[columns[1L]]], sigma = ses[j, ][[columns[1L]]])) *
            psi_function(a = self$ys_front_augmented[i, ][[columns[2L]]], b = self$ys_front_augmented[i, ][[columns[2L]]], mu = means[j, ][[columns[2L]]], sigma = ses[j, ][[columns[2L]]])
        })
        data.table(acq_ehvi = sum(first_summands, na.rm = TRUE) + sum(second_summands, na.rm = TRUE))  # NA is 0
      })
      # FIXME: what about ses = 0
      # FIXME: how to add loop
    }
  )
)

mlr_acqfunctions$add("ehvi", AcqFunctionEHVI)

# emmerich_2016 5.2.1
psi_function = function(a, b, mu, sigma) {
  sigma * dnorm(b, mean = mu, sd = sigma) + (a - mu) * pnorm(b, mean = mu, sd = sigma)
}
