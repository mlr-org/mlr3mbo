#' @title Sequential Multicriteria Bayesian Optimization via Multiple Acquisition Functions
#'
#' @description
#' MBO loop function for multicriteria Bayesian optimization via multiple acquisition functions.
#' Acquisition functions are calculated separately per target variable and a multicriteria overall acquisition function is constructed internally.
#' This overall acquisition function is then optimized and a batch of pareto optimal candidates are proposed for evaluation.
#' Normally used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstanceMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param surrogates (`NULL` | `lists` of [SurrogateLearner]s)\cr
#'   `list` of [SurrogateLearner]s to be used as surrogates.
#'   If `NULL`, multiple \code{default_surrogate(instance, n_learner = 1)} are used.
#' @param acq_functions (`NULL` | `list` of [AcqFunction]s).\cr
#'   `list` of [AcqFunction]s to be used as acquisition functions.
#'   If `NULL`, multiple [AcqFunctionEI]s are used.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   Must have multi-crit as a property.
#' @param q (`integer(1)`)\cr
#'   Limit to batch size, i.e., `q` pareto optimal candidates should be proposed for evaluation.
#'   Note that the upper limit of the batch size is always given by the number of pareto optimal points found during optimization of the overall acquisition function.
#'   If more pareto optimal candidates are available than `q` should be proposed, `q` potential candidates are choosen at random.
#'   Default is `1`.
#'
#' @note
#' * If not specified, the order of which [SurrogateLearner] and [AcqFunction] correspond to which target variable
#'   is based on the `$cols_y` field of the `$archive` of the `instance`.
#' * If `surrogates` is `NULL` but the `acq_functions` each contain a `$surrogate`, these [SurrogateLearner]s are used.
#' * You can pass `surrogates` that were not given the [bbotk::Archive] of the `instance` during initialization.
#'   In this case, the [bbotk::Archive] of the given `instance` is set during execution.
#' * Similarly, you can pass `acq_functions` that were not given a `surrogate` during initialization.
#' * The actual multicriteria `acq_function` to be optimized by the `acq_optimizer` is constructed and set internally.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @family Loop Function
#' @export
#' @examples
#' library(bbotk)
#' library(paradox)
#' library(mlr3learners)
#'
#' fun = function(xs) {
#'   list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#' }
#' domain = ps(x = p_dbl(lower = -10, upper = 10))
#' codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#' objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#' terminator = trm("evals", n_evals = 10)
#'
#' instance = OptimInstanceMultiCrit$new(
#'   objective = objective,
#'   terminator = terminator,
#' )
#'
#' bayesopt_mego(instance)
bayesopt_mego = function(
    instance,
    init_design_size = NULL,
    surrogates = NULL,
    acq_functions = NULL,
    acq_optimizer = NULL,
    q = 1L    
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_list(surrogates, types = "SurrogateLearner", null.ok = TRUE)
  assert_list(acq_functions, types = "AcqFunction", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)

  # FIXME: if surrogates and acq_functions are provided, they must be of equal length

  surrogates = surrogates %??% map(acq_functions, "surrogate")
  if (all(map_lgl(surrogates, is.null))) surrogates = NULL  # FIXME: ugly but we can have NULL, list(), or list(NULL, ...)

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4 * d
  if (is.null(surrogates)) {
    surrogates = map(instance$archive$cols_y, function(y_col) {
      default_surrogate(instance, n_learner = 1L)
    })
  }
  if (is.null(acq_functions)) {
    acq_functions = map(instance$archive$cols_y, function(y_col) {
      AcqFunctionEI$new()
    })
  }
  for (i in seq_along(instance$archive$cols_y)) { 
    surrogates[[i]]$archive = archive
    surrogates[[i]]$y_cols = instance$archive$cols_y[[i]]
    acq_functions[[i]]$surrogate = surrogates[[i]]
  }

  # generate a multicriteria acquisition function based on the singlecriteria ones
  acq_function_domain = instance$search_space$clone(deep = TRUE)
  acq_function_domain$trafo = NULL

  acq_function_codomain = ParamSet$new(map(acq_functions, function(acq_function) {
    id = paste0(acq_function$id, "_", acq_function$surrogate$y_cols)
    ParamDbl$new(id = id, tags = unlist(acq_function$codomain$tags, use.names = FALSE))
  }))

  acq_function = AcqFunctionMulti$new("multi", acq_functions = acq_functions, domain = acq_function_domain, codomain = acq_function_codomain, archive = archive)
  class(acq_function) = c(class(acq_function), "AcqFunction")  # FIXME:

  if (is.null(acq_optimizer)) acq_optimizer = default_acqopt(acq_function)
  acq_optimizer$acq_function = acq_function

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  }

  # loop
  repeat {
    xdt = tryCatch({
      # FIXME: this should go into multi class below
      invisible(map(acq_functions, function(acq_function) {
        acq_function$surrogate$update()
        acq_function$update()
      }))
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      lg$info("Proposing a randomly sampled point")
      SamplerUnif$new(domain)$sample(1L)$data
    })

    if (nrow(xdt) > 1L) {
      xdt = xdt[sample(.N, size = q), ]
    }

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}



AcqFunctionMulti = R6Class("AcqFunctionMulti",
  inherit = bbotk::Objective,
  public = list(

    #' @field acq_functions (`list` of [AcqFunction]s).
    acq_functions = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param acq_functions (`list` of [AcqFunction]s).
    #' @param domain ([paradox::ParamSet]).
    #' @param codomain ([paradox::ParamSet]).
    #' @param archive ([bbotk::Archive]).
    initialize = function(id, acq_functions, domain, codomain, archive) {
      assert_string(id)
      self$acq_functions = assert_list(acq_functions, type = "AcqFunction")
      private$.archive = assert_r6(archive, "Archive")

      super$initialize(
        id = id,
        domain = domain,
        codomain = codomain,
        # FIXME: constants?
      )
    },

    #' @description
    #' Evaluates multiple input values received as a list, converted to a `data.table()` on the
    #' objective function. Missing columns in xss are filled with `NA`s in `xdt`.
    #'
    #' @param xss (`list()`)\cr
    #'   A list of lists that contains multiple x values, e.g.
    #'   `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.
    #'
    #' @return [data.table::data.table()] that contains one y-column for single-criteria functions
    #' and multiple y-columns for multi-criteria functions, e.g.
    #' `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_many = function(xss) {
      if (self$check_values) lapply(xss, self$domain$assert)
      res = invoke(private$.fun, rbindlist(xss, use.names = TRUE, fill = TRUE), .args = self$constants$values)
      if (self$check_values) self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      res
    },

    #' @description
    #' Evaluates multiple input values on the objective function supplied by the user.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   Set of untransformed points / points from the *search space*.
    #'   One point per row, e.g. `data.table(x1 = c(1, 3), x2 = c(2, 4))`.
    #'   Column names have to match ids of the `search_space`.
    #'   However, `xdt` can contain additional columns.
    #'
    #' @return data.table::data.table()] that contains one y-column for single-criteria functions
    #' and multiple y-columns for multi-criteria functions, e.g.
    #' `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_dt = function(xdt) {
      if (self$check_values) self$domain$assert_dt(xdt)
      res = invoke(private$.fun, xdt, .args = self$constants$values)
      if (self$check_values) self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      res
    }
  ),

  active = list(
    #' @field archive ([bbotk::Archive])\cr
    #'   Points to the [bbotk::Archive] of the [Surrogate]s of the [AcqFunction]s
    archive = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("archive is read-only.")
      private$.archive
    },

    #' @field fun (`function`)\cr
    #'   Objective function.
    fun = function(lhs) {
      if (!missing(lhs) && !identical(lhs, private$.fun)) stop("fun is read-only.")
      private$.fun
    }
  ),

  private = list(
    .archive = NULL,

    .fun = function(xdt) {
      xss = transform_xdt_to_xss(xdt, self$domain)
      setNames(map_dtc(self$acq_functions, function(acqf) acqf$eval_many(xss)), nm = self$codomain$ids())
    }
  )
)

