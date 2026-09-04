#' @title Partition a Search Space into Homogeneous Subspaces
#'
#' @description
#' Splits a hierarchical search space into a list of subspaces along the levels of a single categorical parameter,
#' e.g., the branching parameter of an [mlr3pipelines::Graph] that selects the learner.
#'
#' Each subspace contains the partitioning parameter restricted to the levels of the respective group,
#' all parameters that do not depend on the partitioning parameter,
#' and all parameters whose dependencies can still be satisfied by at least one level of the group.
#' Parameters that can never be active within a group are dropped, which makes the resulting subspaces
#' considerably lower-dimensional and much less conditional than the original search space.
#'
#' The resulting subspaces are the input of [OptimizerADBOSubspaces] and [TunerADBOSubspaces].
#'
#' @param search_space ([paradox::ParamSet])\cr
#'   Search space to partition.
#' @param param (`character(1)`)\cr
#'   Id of the parameter along which the search space is partitioned.
#'   Must be a `ParamFct` of `search_space`.
#' @param groups (named `list()` of `character()`)\cr
#'   Groups of levels of `param`.
#'   The names of the list are used as the ids of the resulting subspaces.
#'   The groups must be disjoint and must cover all levels of `param`.
#'   Default is `NULL`, i.e., every level of `param` becomes a subspace of its own named like the level.
#'
#' @return (named `list()` of [paradox::ParamSet]) with one element per group of `groups`.
#'
#' @note
#' The extra trafo and the constraint of `search_space` are carried over to every subspace unchanged.
#' If they reference parameters that are dropped from a subspace, they must be able to cope with those parameters
#' being absent.
#'
#' @export
#' @examples
#' library(paradox)
#'
#' search_space = ps(
#'   learner = p_fct(c("tabpfn", "ranger", "xgboost")),
#'   tabpfn.n_estimators = p_int(1, 16, depends = learner == "tabpfn"),
#'   ranger.mtry_ratio = p_dbl(0, 1, depends = learner == "ranger"),
#'   xgboost.eta = p_dbl(1e-4, 1, logscale = TRUE, depends = learner == "xgboost"),
#'   subsample = p_dbl(0.1, 1)
#' )
#'
#' subspaces = partition_search_space(
#'   search_space,
#'   param = "learner",
#'   groups = list(gpu = "tabpfn", cpu = c("ranger", "xgboost"))
#' )
#'
#' subspaces$gpu$ids()
#' subspaces$cpu$ids()
#'
#' # one subspace per learner
#' names(partition_search_space(search_space, param = "learner"))
partition_search_space = function(search_space, param, groups = NULL) {
  assert_r6(search_space, classes = "ParamSet")
  assert_choice(param, choices = search_space$ids())
  if (search_space$class[[param]] != "ParamFct") {
    stopf("Parameter '%s' must be a 'ParamFct' but is a '%s'.", param, search_space$class[[param]])
  }
  levels = search_space$levels[[param]]
  groups = groups %??% set_names(as.list(levels), levels)
  assert_list(groups, types = "character", min.len = 1L, names = "unique", any.missing = FALSE)
  walk(groups, function(levels) assert_character(levels, min.len = 1L, unique = TRUE))

  all_levels = unlist(groups, use.names = FALSE)
  if (anyDuplicated(all_levels)) {
    stopf("The groups must be disjoint but level(s) %s appear in more than one group.",
      str_collapse(unique(all_levels[duplicated(all_levels)]), quote = "'"))
  }
  assert_set_equal(all_levels, levels, .var.name = sprintf("levels of the groups of '%s'", param))

  set_names(map(names(groups), function(group) {
    subset_search_space(search_space, param = param, levels = groups[[group]])
  }), names(groups))
}

# subset `search_space` to the parameters that can be active if `param` is restricted to `levels`
# and restrict the levels of `param` accordingly
subset_search_space = function(search_space, param, levels) {
  # `on` is a reserved argument of `[.data.table`, so the dependency table is taken apart into plain vectors
  deps = search_space$deps
  child = deps$id
  parent = deps$on
  on_param = parent == param
  unsatisfiable = child[on_param][!map_lgl(deps$cond[on_param], function(cond) any(condition_test(cond, levels)))]

  # parameters that can never be active: those with an unsatisfiable condition on `param` and,
  # transitively, everything depending on them (dependencies in paradox are conjunctive)
  dropped = character()
  repeat {
    new = setdiff(union(unsatisfiable, child[parent %in% dropped]), dropped)
    if (!length(new)) break
    dropped = c(dropped, new)
  }

  subspace = search_space$subset(setdiff(search_space$ids(), dropped))

  # restrict the levels of the partitioning parameter itself
  domains = subspace$domains
  domain = copy(domains[[param]])
  set(domain, j = "levels", value = list(levels))
  set(domain, j = "grouping", value = str_collapse(levels, sep = ",", quote = '"'))
  domains[[param]] = domain

  restricted = ParamSet$new(domains)
  restricted$extra_trafo = subspace$extra_trafo
  restricted$constraint = subspace$constraint
  restricted
}

# `condition_test()` is the (internal) paradox generic that evaluates a dependency condition on a vector of values
condition_test = function(cond, x) {
  fun = utils::getFromNamespace("condition_test", ns = "paradox")
  fun(cond, x)
}

# rows of `data` that lie within `subspace`, i.e., every parameter of the subspace is either inactive (`NA`) or
# within the support of the subspace; rows of other subspaces are excluded via the restricted levels of the
# partitioning parameter
subspace_contains = function(subspace, data) {
  ids = intersect(subspace$ids(), names(data))
  keep = rep(TRUE, nrow(data))
  for (id in ids) {
    value = data[[id]]
    within = if (subspace$is_categ[[id]]) {
      value %in% subspace$levels[[id]]
    } else if (subspace$is_number[[id]]) {
      value >= subspace$lower[[id]] & value <= subspace$upper[[id]]
    } else {
      rep(TRUE, length(value))
    }
    keep = keep & (is.na(value) | within)
  }
  keep
}

# all configurations of a subspace without numeric parameters, respecting its dependencies;
# `NULL` if the subspace has numeric parameters or more than `max_configurations` configurations,
# so that the grid of a large discrete subspace is never materialized
subspace_grid = function(subspace, max_configurations = 10000) {
  if (prod(subspace$nlevels) > max_configurations) {
    return(NULL)
  }
  generate_design_grid(subspace)$data
}

# whether all `n_configurations` configurations of a subspace are among its evaluated points `xdt`
subspace_exhausted = function(subspace, xdt, n_configurations) {
  if (!is.finite(n_configurations) || !nrow(xdt)) {
    return(FALSE)
  }
  uniqueN(xdt[, subspace$ids(), with = FALSE]) >= n_configurations
}

# random design of `n` points of a subspace; a subspace with finitely many configurations is enumerated instead and
# capped at `n`, so that no configuration is evaluated twice
generate_design_subspace = function(subspace, n) {
  grid = subspace_grid(subspace)
  if (is.null(grid)) {
    return(generate_design_random(subspace, n = n)$data)
  }
  grid[sample.int(nrow(grid), min(n, nrow(grid)))]
}
