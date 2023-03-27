#' @title Acquisition Function Expected Hypervolume Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ehvi
#'
#' @description
#' Exact Expected Hypervolume Improvement.
#' Calculates the exact expected hypervolume improvement in the case of two objectives.
#' See Emmerich et al. (2016) for details.
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
    #'   Signs are corrected with respect to assuming minimization of objectives.
    ys_front = NULL,

    #' @field ref_point (`numeric()`)\cr
    #'   Reference point.
    #'   Signs are corrected with respect to assuming minimization of objectives.
    ref_point = NULL,

    #' @field ys_front_augmented (`matrix()`)\cr
    #'   Augmented approximated Pareto front. Sorted by the first objective.
    #'   Signs are corrected with respect to assuming minimization of objectives.
    ys_front_augmented = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearnerCollection]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearnerCollection", null.ok = TRUE)
      super$initialize("acq_ehvi", surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", label = "EHVI", man = "mlr3mbo::mlr_acqfunctions_ehvi")
    },

    #' @description
    #' Updates acquisition function and sets `ys_front`, `ref_point`.
    update = function() {
      n_obj = length(self$archive$cols_y)
      if (n_obj > 2L) {
        stopf("%s only works for exactly two objectives.", self$label)
      }
      ys = self$archive$data[, self$archive$cols_y, with = FALSE]
      for (column in self$archive$cols_y) {
        set(ys, j = column, value = ys[[column]] * self$surrogate_max_to_min[[column]])  # assume minimization
      }
      ys = as.matrix(ys)

      self$ref_point = apply(ys, MARGIN = 2L, FUN = max) + 1  # offset = 1 like in mlrMBO

      self$ys_front = self$archive$best()[, self$archive$cols_y, with = FALSE]
      for (column in self$archive$cols_y) {
        set(self$ys_front, j = column, value = self$ys_front[[column]] * self$surrogate_max_to_min[[column]])  # assume minimization
      }
      setorderv(self$ys_front, cols = self$archive$cols_y[1L], order = -1L)

      ys_front_augmented = rbind(t(setNames(c(self$ref_point[1L], - Inf), nm = self$archive$cols_y)), self$ys_front, t(setNames(c(- Inf, self$ref_point[2L]), nm = self$archive$cols_y)))

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

      # Emmerich et al. (2016) 5.2.1 (16) but vectorized over candidate points
      first_summands = map(seq_len(nrow(self$ys_front_augmented))[-1L], function(i) {
        tmp = (self$ys_front_augmented[i - 1L, ][[columns[1L]]] - self$ys_front_augmented[i, ][[columns[1L]]]) *
          pnorm(self$ys_front_augmented[i, ][[columns[1L]]], mean = means[[columns[1L]]], sd = ses[[columns[1L]]]) *
          psi_function(a = self$ys_front_augmented[i, ][[columns[2L]]], b = self$ys_front_augmented[i, ][[columns[2L]]], mu = means[[columns[2L]]], sigma = ses[[columns[2L]]])
        tmp[is.na(tmp)] = 0  # NA is 0
        tmp
      })
      second_summands = map(seq_len(nrow(self$ys_front_augmented))[-1L], function(i) {
        tmp = (psi_function(a = self$ys_front_augmented[i - 1L, ][[columns[1L]]], b = self$ys_front_augmented[i - 1L, ][[columns[1L]]], mu = means[[columns[1L]]], sigma = ses[[columns[1L]]]) -
         psi_function(a = self$ys_front_augmented[i - 1L, ][[columns[1L]]], b = self$ys_front_augmented[i, ][[columns[1L]]], mu = means[[columns[1L]]], sigma = ses[[columns[1L]]])) *
         psi_function(a = self$ys_front_augmented[i, ][[columns[2L]]], b = self$ys_front_augmented[i, ][[columns[2L]]], mu = means[[columns[2L]]], sigma = ses[[columns[2L]]])
        tmp[is.na(tmp)] = 0  # NA is 0
        tmp
      })
      ehvi = Reduce("+", first_summands) + Reduce("+", second_summands)
      ehvi = ifelse(apply(ses, MARGIN = 1L, FUN = function(se) any(se < 1e-20)), 0, ehvi)
      data.table(acq_ehvi = ehvi)
    }
  )
)

mlr_acqfunctions$add("ehvi", AcqFunctionEHVI)

# Emmerich et al. (2016) psi helper function 5.2.1
psi_function = function(a, b, mu, sigma) {
  sigma * dnorm(b, mean = mu, sd = sigma) + (a - mu) * pnorm(b, mean = mu, sd = sigma)
}
