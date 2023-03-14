generate_acq_codomain = function(codomain, id, direction = "same") {
  assert_choice(direction, c("same", "minimize", "maximize"))
  if (direction == "same") {
    if (codomain$length > 1L) {
      stop("Not supported yet.")  # FIXME: But should be?
    }
    tags = codomain$params[[1L]]$tags
    tags = tags[tags %in% c("minimize", "maximize")]  # only filter out the relevant one
  } else {
    tags = direction
  }
  codomain = ParamSet$new(list(
    ParamDbl$new(id, tags = tags)
  ))
  return(codomain)
}

archive_xy = function(archive) {
  archive$data[, c(archive$cols_x, archive$cols_y), with = FALSE]
}

archive_x = function(archive) {
  archive$data[, archive$cols_x, with = FALSE]
}

# during surrogate prediction it may have happened that whole columns where dropped (e.g., during focussearch if the search space was shrunk)
fix_xdt_missing = function(xdt, x_cols, archive) {
  missing = x_cols[x_cols %nin% colnames(xdt)]
  types = map_chr(missing, function(x) typeof(archive$data[[x]]))
  NA_types = list(double = NA_real_, integer = NA_integer_, character = NA_character_, logical = NA)[types]
  for (i in seq_along(missing)) {
    xdt[, eval(missing[i]) := NA_types[i]]
  }
  assert_subset(x_cols, colnames(xdt))
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

surrogate_mult_max_to_min = function(codomain, y_cols) {
  mult = map_int(y_cols, function(y_col) {
    mult = if (y_col %in% codomain$ids()) {
      if(has_element(codomain$tags[[y_col]], "maximize")) -1L else 1L
    } else {
      1L
    }
  })
  setNames(mult, nm = y_cols)
}

mult_max_to_min = function(codomain) {
  ifelse(map_lgl(codomain$tags, has_element, "minimize"), 1, -1)
}

# used in AcqOptimizer
# FIXME: currently only supports singlecrit acqfunctions
get_best_not_evaluated = function(instance, evaluated) {
  data = copy(instance$archive$data[, c(instance$archive$cols_x, "x_domain", instance$archive$cols_y), with = FALSE])
  evaluated = copy(evaluated)
  already_evaluated_id = ".already_evaluated"
  while (already_evaluated_id %in% c(instance$archive$cols_x, "x_domain", instance$archive$cols_y)) {
    already_evaluated_id = paste0(".", already_evaluated_id)
  }
  data[, eval(already_evaluated_id) := FALSE][evaluated, eval(already_evaluated_id) := TRUE, on = instance$archive$cols_x]
  candidates = data[get(already_evaluated_id) == FALSE]
  if (nrow(candidates) == 0L) {
    stop("All candidates were already evaluated.")
  }
  candidates[[instance$archive$cols_y]] = candidates[[instance$archive$cols_y]] * instance$objective_multiplicator[instance$archive$cols_y]
  xdt = setorderv(candidates, cols = instance$archive$cols_y)[1L, ]
  xdt[[instance$archive$cols_y]] = xdt[[instance$archive$cols_y]] * instance$objective_multiplicator[instance$archive$cols_y]
  xdt
}

catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}

set_collapse = function(x) {
  if (length(x) == 0L) return("{}")
  sprintf("{'%s'}", paste0(unique(x), collapse = "','"))
}

check_attributes = function(x, attribute_names) {
  qassert(attribute_names, rules = "a")
  if (any(attribute_names %nin% names(attributes(x)))) {
    return(sprintf("Attributes must include '%s' but is '%s'", set_collapse(attribute_names), set_collapse(names(attributes(x)))))
  }
  return(TRUE)
}

check_instance_attribute = function(x) {
  if (length(intersect(c("single-crit", "multi-crit"), attr(x, "instance"))) == 0L) {
    return(sprintf("'instance' attribute must be a subset of '%s' but is '%s'", set_collapse(c("single-crit", "multi-crit")), set_collapse(attr(x, "instance"))))
  }
  return(TRUE)
}

assert_loop_function = function(x, .var.name = vname(x)) {
  if (is.null(x)) return(x)
  # NOTE: this is buggy in checkmate; assert should always return x invisible not a TRUE as is the case here
  assert(check_class(x, classes = "loop_function"),
         check_function(x, args = c("instance", "surrogate", "acq_function", "acq_optimizer")),
         check_attributes(x, attribute_names = c("id", "label", "instance", "man")),
         check_instance_attribute(x),
         combine = "and", .var.name = .var.name)
  x
}

assert_xdt = function(xdt) {
  assert_data_table(xdt)
}

task_rm_backend = function(task) {
  # fix task hash
  ee = get_private(task)
  ee$.hash = force(task$hash)
  ee$.col_hashes = force(task$col_hashes)

  # NULL backend
  task$backend = NULL

  task
}
