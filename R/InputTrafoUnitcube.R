#' @title Input Transformation Unitcube
#'
#' @include mlr_input_trafos.R
#'
#' @description
#' Input transformation that performs for each numeric and integer feature min-max scaling to `[\0, 1\]`
#' based on the boundaries of the search space.
#'
#' @family Input Transformation
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
#'   input_trafo = it("unitcube")
#'
#'   surrogate = srlrn(learner, input_trafo = input_trafo, archive = instance$archive)
#'
#'   surrogate$update()
#'
#'   surrogate$input_trafo$state
#'
#'   surrogate$predict(data.table(x = c(-1, 0, 1)))
#' }
InputTrafoUnitcube = R6Class(
  "InputTrafoUnitcube",
  inherit = InputTrafo,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    initialize = function() {
      super$initialize(label = "Unitcube", man = "mlr3mbo::mlr_input_trafos_unitcube")
    },

    #' @description
    #' Learn the transformation based on observed data and update parameters in `$state`.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_x`.
    update = function(xdt) {
      parameters = names(which(self$search_space$is_number)) # numeric or integer
      bounds_finite = is.finite(self$search_space$lower[parameters]) & is.finite(self$search_space$upper[parameters])
      if (!all(bounds_finite)) {
        error_config(
          "InputTrafoUnitcube requires finite bounds for all numeric parameters but the bounds of '%s' are not finite.",
          str_collapse(names(which(!bounds_finite)))
        )
      }
      private$.state = list()
    },

    #' @description
    #' Perform the transformation.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   Data. One row per observation with at least columns `$cols_x`.
    #'
    #' @return [data.table::data.table()] with the transformation applied to the columns `$cols_x` (if applicable)
    #'   or a subset thereof.
    transform = function(xdt) {
      if (is.null(self$state)) {
        stop("$state is not set. Missed to call $update()?")
      }
      xdt = copy(xdt)
      parameters = names(which(self$search_space$is_number)) # numeric or integer
      assert_subset(parameters, self$cols_x)
      for (parameter in parameters) {
        lower = self$search_space$lower[[parameter]]
        upper = self$search_space$upper[[parameter]]
        # a degenerate dimension carries no information and is mapped to the center of the unit interval
        value = if (lower == upper) {
          rep(0.5, nrow(xdt))
        } else {
          (xdt[[parameter]] - lower) / (upper - lower)
        }
        set(xdt, j = parameter, value = value)
      }
      xdt
    }
  ),

  active = list(
    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled if at least one of the packages is not installed,
    #'   but loaded (not attached) later on-demand via [requireNamespace()].
    packages = function(rhs) {
      if (missing(rhs)) {
        character(0)
      } else {
        stop("$packages is read-only.")
      }
    }
  )
)

mlr_input_trafos$add("unitcube", InputTrafoUnitcube)
