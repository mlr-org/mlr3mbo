generate_acq_codomain = function(surrogate, id, direction = "same") {
  assert_r6(surrogate$archive, "Archive")
  assert_string(id)
  assert_choice(direction, choices = c("same", "minimize", "maximize"))
  if (direction == "same") {
    assert(surrogate$archive$codomain$length == 1L)
    tags = surrogate$archive$codomain$tags[[1L]]
    tag = tags[tags %in% c("minimize", "maximize")]  # only filter out the relevant one
  } else {
    tag = direction
  }
  codomain = do.call(ps, structure(list(p_dbl(tags = tag)), names = id))
  codomain
}

generate_acq_multi_codomain = function(surrogate, acq_functions) {
  acq_functions = unname(acq_functions)  # needed for c of ParamSets to keep original ids
  codomain = do.call(c, map(acq_functions, function(acq_function) acq_function$codomain))
  if (any(codomain$tags == "same")) {
    assert(surrogate$archive$codomain$length == 1L)
  }
  codomain$tags = structure(as.list(rep("maximize", length(acq_functions))), names = map_chr(acq_functions, function(acq_function) acq_function$id))
  codomain
}

generate_acq_domain = function(surrogate) {
  assert_archive(surrogate$archive)
  if ("set_id" %in% names(ps())) {
    # old paradox
    domain = surrogate$archive$search_space$clone(deep = TRUE)$subset(surrogate$cols_x)
    domain$trafo = NULL
  } else {
    # get "domain" objects, set their .trafo-entry to NULL individually
    dms = lapply(surrogate$archive$search_space$domains[surrogate$cols_x], function(x) {
      x$.trafo[1] = list(NULL)
      x
    })
    domain = do.call(ps, dms)
  }
  domain
}

archive_xy = function(archive) {
  archive$data[, c(archive$cols_x, archive$cols_y), with = FALSE]
}

archive_x = function(archive) {
  archive$data[, archive$cols_x, with = FALSE]
}

# during surrogate prediction it may have happened that whole columns where dropped (e.g., during focussearch if the search space was shrunk)
fix_xdt_missing = function(xdt, cols_x, archive) {
  missing = cols_x[cols_x %nin% colnames(xdt)]
  types = map_chr(missing, function(x) typeof(archive$data[[x]]))
  NA_types = list(double = NA_real_, integer = NA_integer_, character = NA_character_, logical = NA)[types]
  for (i in seq_along(missing)) {
    xdt[, eval(missing[i]) := NA_types[i]]
  }
  assert_subset(cols_x, colnames(xdt))
  xdt
}

# calculate all possible weights (lambdas) for given s parameter and dimensionality k taken von mlrMBO
calculate_parego_weights = function(s, k) {
  fun = function(s, k) {
    if (k == 1L)
      list(s)
    else
      unlist(lapply(0:s, function(i) Map(c, i, fun(s - i, k - 1L))), recursive = FALSE)
  }
  matrix(unlist(fun(s, k)), ncol = k, byrow = TRUE) / s
}

surrogate_mult_max_to_min = function(surrogate) {
  codomain = surrogate$archive$codomain
  cols_y = surrogate$cols_y
  mult = map_int(cols_y, function(col_y) {
    mult = if (col_y %in% surrogate$archive$codomain$ids()) {
      if (has_element(surrogate$archive$codomain$tags[[col_y]], "maximize")) {
        -1L
      } else {
        1L
      }
    } else {
      1L
    }
  })
  setNames(mult, nm = cols_y)
}

mult_max_to_min = function(codomain) {
  ifelse(map_lgl(codomain$tags, has_element, "minimize"), yes = 1L, no = -1L)
}

# used in AcqOptimizer
get_best= function(instance, is_multi_acq_function, evaluated, n_select, not_already_evaluated = TRUE) {
  data = copy(instance$archive$data[, c(instance$archive$cols_x, "x_domain", instance$archive$cols_y), with = FALSE])
  evaluated = copy(evaluated)
  already_evaluated_id = ".already_evaluated"
  set(data, j = already_evaluated_id, value = FALSE)
  data[evaluated, eval(already_evaluated_id) := TRUE, on = instance$archive$cols_x]
  set(instance$archive$data, j = already_evaluated_id, value = data[[already_evaluated_id]])
  if (not_already_evaluated) {
    not_already_evaluated = which(data[[already_evaluated_id]] == FALSE)
    if (length(not_already_evaluated) < n_select) {
      stopf("Less then `n_select` (%i) candidate points found during acquisition function optimization were not already evaluated.", n_select)
    }
    instance$archive$data = instance$archive$data[not_already_evaluated, ]
  }
  if (is_multi_acq_function) {
    instance$archive$nds_selection(n_select = n_select)
  } else {
    instance$archive$best(n_select = n_select)
  }
}

catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}

set_collapse = function(x) {
  if (length(x) == 0L) {
    return("{}")
  }
  sprintf("{'%s'}", paste0(unique(x), collapse = "','"))
}

check_attributes = function(x, attribute_names) {
  qassert(attribute_names, rules = "a")
  if (any(attribute_names %nin% names(attributes(x)))) {
    return(sprintf("Attributes must include '%s' but is '%s'", set_collapse(attribute_names), set_collapse(names(attributes(x)))))
  }
  TRUE
}

check_instance_attribute = function(x) {
  if (length(intersect(c("single-crit", "multi-crit"), attr(x, "instance"))) == 0L) {
    return(sprintf("'instance' attribute must be a subset of '%s' but is '%s'", set_collapse(c("single-crit", "multi-crit")), set_collapse(attr(x, "instance"))))
  }
  TRUE
}

check_learner_surrogate = function(learner) {
  if (test_r6(learner, classes = "Learner")) {
    return(TRUE)
  } else {
    if (inherits(learner, what = "list") && all(map_lgl(learner, .f = function(x) test_r6(x, classes = "Learner")))) {
      return(TRUE)
    }
  }
  "Must inherit from class 'Learner' or be a list of elements inheriting from class 'Learner'"
}

assert_loop_function = function(x, .var.name = vname(x)) {
  if (is.null(x)) {
    return(x)
  }
  # NOTE: this is buggy in checkmate; assert should always return x invisible not TRUE as is the case here
  assert(check_class(x, classes = "loop_function"),
    check_function(x, args = c("instance", "surrogate", "acq_function", "acq_optimizer")),
    check_attributes(x, attribute_names = c("id", "label", "instance", "man")),
    check_instance_attribute(x),
    combine = "and", .var.name = .var.name
  )
  x
}

assert_xdt = function(xdt) {
  assert_data_table(xdt)
}

assert_learner_surrogate = function(x, .var.name = vname(x)) {
  # NOTE: this is buggy in checkmate; assert should always return x invisible not TRUE as is the case here
  assert(check_learner_surrogate(x), .var.name = .var.name)

  x
}

#' Check if Redis Server is Available
#'
#' Attempts to establish a connection to a Redis server using the \CRANpkg{redux} package
#' and sends a `PING` command. Returns `TRUE` if the server is available and
#' responds appropriately, `FALSE` otherwise.
#'
#' @return (`logical(1)`)
#' @export
#' @examples
#' if (redis_available()) {
#'   # Proceed with code that requires Redis
#'   message("Redis server is available.")
#' } else {
#'   message("Redis server is not available.")
#' }
redis_available = function() {
  requireNamespace("rush")
  tryCatch({
    rush::rsh()
    config = redux::redis_config()
    server = redux::hiredis(config)
    ping = server$PING()
    ping == "PONG"
  }, error = function(e) {
    FALSE
  })
}
