#' @title Output Transformation Standardization
#'
#' @include mlr_output_trafos.R
#'
#' @description
#' Output transformation that performs standardization to zero mean and unit variance.
#'
#' @family Output Transformation
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   xdt = generate_design_random(instance$search_space, n = 4)$data
#'
#'   instance$eval_batch(xdt)
#'
#'   learner = default_gp()
#'
#'   output_trafo = ot("standardize", invert_posterior = TRUE)
#'
#'   surrogate = srlrn(learner, output_trafo = output_trafo, archive = instance$archive)
#'
#'   surrogate$update()
#'
#'   surrogate$output_trafo$state
#'
#'   surrogate$predict(data.table(x = c(-1, 0, 1)))
#'
#'   surrogate$output_trafo$invert_posterior = FALSE
#'
#'   surrogate$predict(data.table(x = c(-1, 0, 1)))
#' }
OutputTrafoStandardize = R6Class("OutputTrafoStandardize",
  inherit = OutputTrafo,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param invert_posterior (`logical(1)`)\cr
    #'  Should the posterior predictive distribution be inverted when used within a [SurrogateLearner] or [SurrogateLearnerCollection]?
    initialize = function(invert_posterior = TRUE) {
      super$initialize(invert_posterior = invert_posterior, label = "Standardize", man = "mlr3mbo::mlr_output_trafos_standardize")
    },

    #' @description
    #' Learn the transformation based on observed data and update parameters in `$state`.
    #'
    #' @param ydt ([data.table::data.table()])\cr
    #'   Data. One row per observation with columns `$cols_y`.
    update = function(ydt) {
      state = map(self$cols_y, function(col_y) {
        list(mu = mean(ydt[[col_y]]), sigma = sd(ydt[[col_y]]))
      })
      state = setNames(state, nm = self$cols_y)
      private$.state = state
    },

    #' @description
    #' Perform the transformation.
    #'
    #' @param ydt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_y`.
    #'
    #' @return [data.table::data.table()] with the transformation applied to the columns `$cols_y`.
    transform = function(ydt) {
      if (is.null(self$state)) {
        stop("$state is not set. Missed to call $update()?")
      }
      ydt = copy(ydt)
      for (col_y in self$cols_y) {
        set(ydt, j = col_y, value = (ydt[[col_y]] - self$state[[col_y]]$mu) / self$state[[col_y]]$sigma)
      }
      ydt
    },

    #' @description
    #' Perform the inverse transformation on a posterior predictive distribution characterized by the first and second moment.
    #'
    #' @param pred ([data.table::data.table()])\cr
    #'   Data. One row per observation characterizing a posterior predictive distribution with the columns `mean` and `se`.
    #'   Can also be a named list of [data.table::data.table()] with posterior predictive distributions for multiple targets corresponding to (`cols_y`).
    #'
    #' @return [data.table::data.table()] with the inverse transformation applied to the columns `mean` and `se`.
    #'   In the case of the input being a named list of [data.table::data.table()], the output will be a named list of [data.table::data.table()] with the inverse transformation applied to the columns `mean` and `se`.
    inverse_transform_posterior = function(pred) {
      if (is.null(self$state)) {
        stop("$state is not set. Missed to call $update()?")
      }
      pred = copy(pred)
      if (length(self$cols_y) == 1L) {
        assert_data_table(pred)
        pred = list(pred)
        pred = setNames(pred, nm = self$cols_y)
      } else {
        assert_list(pred, len = length(self$cols_y))
        assert(all(names(pred) == self$cols_y))
        for (col_y in self$cols_y) {
          assert_data_table(pred[[col_y]])
        }
      }
      for (col_y in self$cols_y) {
      	mean = pred[[col_y]]$mean * self$state[[col_y]]$sigma + self$state[[col_y]]$mu
      	se = pred[[col_y]]$se * self$state[[col_y]]$sigma
      	set(pred[[col_y]], j = "mean", value = mean)
      	set(pred[[col_y]], j = "se", value = se)
      }
      if (length(self$cols_y) == 1L) {
        pred[[self$cols_y]]
      } else {
        pred
      }
    },

    #' @description
    #' Perform the inverse transformation.
    #'
    #' @param ydt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_y`.
    #'
    #' @return [data.table::data.table()] with the inverse transformation applied to the columns `$cols_y`.
    inverse_transform = function(ydt) {
      if (is.null(self$state)) {
        stop("$state is not set. Missed to call $update()?")
      }
      ydt = copy(ydt)
      for (col_y in self$cols_y) {
        set(ydt, j = col_y, value = (ydt[[col_y]] * self$state[[col_y]]$sigma) + self$state[[col_y]]$mu)
      }
      ydt
    }
  ),

  active = list(
    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
    packages = function(rhs) {
      if (missing(rhs)) {
        character(0)
      } else {
        stop("$packages is read-only.")
      }
    }
  )
)

mlr_output_trafos$add("standardize", OutputTrafoStandardize)

