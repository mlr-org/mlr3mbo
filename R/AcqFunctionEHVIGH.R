#' @title Acquisition Function Expected Hypervolume Improvement via Gauss-Hermite Quadrature
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ehvigh
#'
#' @description
#' Expected Hypervolume Improvement.
#' Computed via Gauss-Hermite quadrature.
#'
#' In the case of optimizing only two objective functions [AcqFunctionEHVI] is to be preferred.
#'
#' @section Parameters:
#' * `"k"` (`integer(1)`)\cr
#'   Number of nodes per objective used for the numerical integration via Gauss-Hermite quadrature.
#'   Defaults to `15`.
#'   For example, if two objectives are to be optimized, the total number of nodes will therefore be 225 per default.
#'   Changing this value after construction requires a call to `$update()` to update the `$gh_data` field.
#' * `"r"` (`numeric(1)`)\cr
#'   Pruning rate between 0 and 1 that determines the fraction of nodes of the Gauss-Hermite quadrature rule that are ignored based on their weight value (the nodes with the lowest weights being ignored).
#'   Default is `0.2`.
#'   Changing this value after construction does not require a call to `$update()`.
#'
#' @references
#' * `r format_bib("rahat_2022")`
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
#'   acq_function = acqf("ehvigh", surrogate = surrogate)
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionEHVIGH = R6Class("AcqFunctionEHVIGH",
  inherit = AcqFunction,

  public = list(

    #' @field ys_front (`matrix()`)\cr
    #'   Approximated Pareto front.
    #'   Signs are corrected with respect to assuming minimization of objectives.
    ys_front = NULL,

    #' @field ref_point (`numeric()`)\cr
    #'   Reference point.
    #'   Signs are corrected with respect to assuming minimization of objectives.
    ref_point = NULL,

    #' @field hypervolume (`numeric(1)`).
    #'   Current hypervolume of the approximated Pareto front with respect to the reference point.
    hypervolume = NULL,

    #' @field gh_data (`matrix()`)\cr
    #'   Data required for the Gauss-Hermite quadrature rule in the form of a matrix of dimension (k x 2).
    #'   Each row corresponds to one Gauss-Hermite node (column `"x"`) and corresponding weight (column `"w"`).
    #'   Computed via [fastGHQuad::gaussHermiteData].
    #'   Nodes are scaled by a factor of `sqrt(2)` and weights are normalized under a sum to one constraint.
    gh_data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearnerCollection]).
    #' @param k (`integer(1)`).
    #' @param r (`numeric(1)`).
    initialize = function(surrogate = NULL, k = 15L, r = 0.2) {
      assert_r6(surrogate, "SurrogateLearnerCollection", null.ok = TRUE)
      assert_int(k, lower = 2L)

      constants = ParamSet$new(list(
        ParamInt$new("k", lower = 2L, default = 15L),
        ParamDbl$new("r", lower = 0, upper = 1, default = 0.2)
      ))
      constants$values$k = k
      constants$values$r = r

      super$initialize("acq_ehvigh", constants = constants, surrogate = surrogate, requires_predict_type_se = TRUE, direction = "maximize", packages = c("emoa", "fastGHQuad"), label = "Expected Hypervolume Improvement via GH Quadrature", man = "mlr3mbo::mlr_acqfunctions_ehvigh")
    },

    #' @description
    #' Updates acquisition function and sets `ys_front`, `ref_point`, `hypervolume`, `gh_data`.
    update = function() {
      n_obj = length(self$archive$cols_y)
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
      self$ys_front = as.matrix(self$ys_front)

      self$hypervolume = invoke(emoa::dominated_hypervolume, points = t(self$ys_front), ref = t(self$ref_point))

      self$gh_data = invoke(fastGHQuad::gaussHermiteData, n = self$constants$values$k)  # k because the multi-dimensional grid is created within adjust_gh_data
      self$gh_data$x = self$gh_data$x * sqrt(2)
      self$gh_data$w = self$gh_data$w / sum(self$gh_data$w)
      self$gh_data = do.call(cbind, self$gh_data)
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      constants = list(...)
      r = constants$r
      if (is.null(self$ys_front)) {
        stop("$ys_front is not set. Missed to call $update()?")
      }
      if (is.null(self$ref_point)) {
        stop("$ref_point is not set. Missed to call $update()?")
      }
      if (is.null(self$hypervolume)) {
        stop("$hypervolume is not set. Missed to call $update()?")
      }
      if (is.null(self$gh_data)) {
        stop("$gh_data is not set. Missed to call $update()?")
      }
      ps = self$surrogate$predict(xdt)
      means = as.matrix(map_dtc(ps, "mean"))
      vars = as.matrix(map_dtc(ps, "se")) ^ 2

      ehvi = map_dbl(seq_len(nrow(xdt)), function(i) {
        gh_data = adjust_gh_data(self$gh_data, mu = means[i, ], sigma = diag(vars[i, ]), r = r)
        hvs = map_dbl(seq_along(gh_data$weights), function(j) {
          ys = rbind(self$ys_front, gh_data$nodes[j, ] %*% diag(self$surrogate_max_to_min))
          invoke(emoa::dominated_hypervolume, points = t(ys), ref = t(self$ref_point))
        })
        sum((hvs - self$hypervolume) * gh_data$weights, na.rm = TRUE)
      })

      ehvi = ifelse(apply(sqrt(vars), MARGIN = 1L, FUN = function(se) any(se < 1e-20)), 0, ehvi)
      data.table(acq_ehvigh = ehvi)
    }
  )
)

mlr_acqfunctions$add("ehvigh", AcqFunctionEHVIGH)

adjust_gh_data = function(gh_data, mu, sigma, r) {
  # inspired from https://www.r-bloggers.com/2015/09/notes-on-multivariate-gaussian-quadrature-with-r-code/
  n = nrow(gh_data)
  n_obj = length(mu)
  # idx grows exponentially in n and n_obj
  idx = as.matrix(expand.grid(rep(list(1:n), n_obj)))
  nodes = matrix(gh_data[idx, 1L], nrow = nrow(idx), ncol = n_obj)
  weights = apply(matrix(gh_data[idx, 2L], nrow = nrow(idx), ncol = n_obj), MARGIN = 1L, FUN = prod)
 
  # pruning with pruning rate r 
  if (r > 0) {
    weights_quantile = quantile(weights, probs = r)
    nodes = nodes[weights > weights_quantile, ]
    weights = weights[weights > weights_quantile]
  }
  
  # rotate, scale, translate nodes with error catching
  # rotation will not have an effect unless we support surrogate models modelling correlated objectives
  # for now we still support this more general case and scaling is useful anyways
  nodes = tryCatch(
    {
      eigen_decomp = eigen(sigma) 
      rotation = eigen_decomp$vectors %*% diag(sqrt(eigen_decomp$values))
      nodes = t(rotation %*% t(nodes) + mu)
    }, error = function(ec) nodes
  )

  list(nodes = nodes, weights = weights)
}
