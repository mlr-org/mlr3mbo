#' @title Optimization via Local Search
#'
#' @name mlr_optimizers_local_search
#'
#' @description
#' `OptimizerBatchLocalSearch` class that implements a simple Local Search.
#' Local Search starts by determining the `mu` initial best points present in the [Archive] of the
#' [OptimInstance]. If fewer points than `mu` are present, additional points sampled uniformly at
#' random are evaluated.
#'
#' In each iteration, for each of the `mu` initial best points, `n_points` neighbors are generated
#' by local mutation. Local mutation generates a neighbor by sampling a single parameter that is to
#' be mutated and then proceeds as follows: Double parameters ([paradox::p_dbl]) are mutated via
#' Gaussian mutation (with a prior standardization to `[0, 1]` and retransformation after mutation).
#' Integer parameters ([paradox::p_int]) undergo the same mutation but are rounded to the closest
#' integer after mutation. Categorical parameters ([paradox::p_fct] and [paradox::p_lgl]) are
#' mutated via uniform mutation. Note that parameters that are conditioned on (i.e., they are
#' parents of a [paradox::Condition], see the dependencies of the search space) are not mutated.
#'
#' @section Parameters:
#' \describe{
#' \item{`mu`}{`integer(1)`\cr
#'   Size of the initial best points which are used as a starting points for the Local Search.
#'   Default is `10`.
#' }
#' \item{`n_points`}{`integer(1)`\cr
#'   Number of neighboring points to generate for each of the `mu` best starting points in each
#'   iteration.
#'   Default is `100`.
#' }
#' \item{`sigma`}{`numeric(1)`\cr
#'   Standard deviation used for mutation of numeric parameters.
#'   Default is `0.1`.
#' }
#' }
#'
#' @export
OptimizerBatchLocalSearch = R6Class("OptimizerBatchLocalSearch",
  inherit = bbotk::OptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        mu = p_int(lower = 1L, default = 10L),
        n_points = p_int(lower = 1L, default = 100L),
        sigma = p_dbl(lower = 0L, default = 0.1)
      )
      param_set$values = list(mu = 10L, n_points = 100L, sigma = 0.1)

      super$initialize(
        id = "local_search",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit"), # NOTE: think about multi-crit version
        label = "Local Search",
        man = "mlr3mbo::mlr_optimizers_local_search"
      )
    }
  ),
  private = list(
    .optimize = function(inst) {
      mu = self$param_set$values$mu
      mu_seq = seq_len(mu)
      n_points = self$param_set$values$n_points
      n_points_seq = seq_len(n_points)
      sigma = self$param_set$values$sigma

      # if no reference points in archive, generate mu by sampling uniformly at random
      if (inst$archive$n_evals < mu) {
        data = generate_design_random(inst$search_space, n = mu - inst$archive$n_evals)$data
        inst$eval_batch(data)
      }
      points = inst$archive$best(n_select = mu)[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]

      # we do not mutate parents of conditions
      ids_to_mutate = setdiff(inst$search_space$ids(), unique(inst$search_space$deps$on))
      ids_numeric = intersect(inst$search_space$ids(class = c("ParamDbl", "ParamInt")), ids_to_mutate)
      ids_categorical = intersect(inst$search_space$ids(class = c("ParamLgl", "ParamFct")), ids_to_mutate)
      ids_categorical = intersect(ids_categorical, search_space$ids()[search_space$nlevels > 1])

      point_id = ".point_id"
      while (point_id %in% c(inst$archive$cols_x, inst$archive$cols_y)) {
        point_id = paste0(".", point_id)
      }

      repeat { # iterate until we have an exception from eval_batch
        # generate neighbors
        neighbors = map_dtr(mu_seq, function(i) {
          neighbors_i = map_dtr(n_points_seq, function(j) {
            # NOTE: mutating is currently quite slow because we sample the id to be mutated and the actual mutation for each neighbor and new point
            mutate_point(points[i, inst$archive$cols_x, with = FALSE], search_space = inst$search_space, ids_numeric = ids_numeric, ids_categorical = ids_categorical, sigma = sigma)
          })
          set(neighbors_i, j = point_id, value = i)
        })

        # evaluate neighbors
        inst$eval_batch(neighbors)

        # update points if better neighbor found
        for (i in mu_seq) {
          tmp = inst$archive$data[batch_nr == inst$archive$n_batch & get(point_id) == i]
          difference = (tmp[[inst$archive$cols_y]] * inst$objective_multiplicator) - (points[i, ][[inst$archive$cols_y]] * inst$objective_multiplicator)
          if (any(difference < 0)) {
            best = which.min(difference)
            points[i, ] = tmp[best, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]
          }
        }
      }
    }
  )
)

mutate_point = function(point, search_space, ids_numeric, ids_categorical, sigma) {
  neighbor = copy(point)
  valid_numeric_to_mutate = intersect(names(which(!map_lgl(neighbor, is.na))), ids_numeric)
  valid_cateorical_to_mutate = intersect(names(which(!map_lgl(neighbor, is.na))), ids_categorical)
  id = sample(c(valid_numeric_to_mutate, valid_cateorical_to_mutate), size = 1L)
  neighbor[1L, ][[id]] = mutate(neighbor[1L, ][[id]], subspace = search_space$subspaces(ids = id)[[1]], sigma = sigma)
  neighbor
}

mutate = function(value, subspace, sigma) {
  if (subspace$class %in% c("ParamDbl", "ParamInt")) {
    value_ = (value - subspace$lower) / (subspace$upper - subspace$lower)
    value_ = max(0, min(stats::rnorm(1L, mean = value_, sd = sigma), 1))
    value = (value_ * (subspace$upper - subspace$lower)) + subspace$lower
    if (subspace$class == "ParamInt") {
      value = round(value, 0L)
    }
    value = min(max(value, subspace$lower), subspace$upper)
  } else if (subspace$class %in% c("ParamFct", "ParamLgl")) {
    value = sample(setdiff(subspace$levels[[1]], value), size = 1L)
  }
  value
}

#' @include aaa.R
optimizers[["local_search"]] = OptimizerBatchLocalSearch
