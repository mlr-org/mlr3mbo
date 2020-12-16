generate_acq_codomain = function(codomain, id, direction = "same") {
  assert_choice(direction, c("same", "minimize", "maximize"))
  if (direction == "same") {
    if (codomain$length > 1) {
      stop("not supportet yet")  # FIXME: But should be
    }
    tags = codomain$params[[1]]$tags
    tags = tags[tags %in% c("minimize", "maximize")]  # only filter out the relevant one
  } else {
    tags = direction
  }
  codomain = ParamSet$new(list(
    ParamDbl$new(id, tags = tags)
  ))
  return(codomain)
}

feature_types_to_param_classes = function(feature_types) {
  param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct")
  param_classes[c("logical", "integer", "numeric", "factor") %in% feature_types]
}

archive_xy = function(archive) {
  archive$data()[, c(archive$cols_x, archive$cols_y), with = FALSE]
}

check_gower_distance = function(xdt, previous_xdt, distance_epsilon, compare_to_itself = FALSE) {
  # FIXME: catch some safe warnings, i.e., "skipping variable with zero or non-finite range"
  gower_distance = if (compare_to_itself) {
    if (nrow(xdt) > 1L) {
      map(seq_len(nrow(xdt)), function(i) {
        gower::gower_dist(xdt[-i, ], xdt[i, ], nthread = 1L)  # NOTE: no parallel execution for now
      })
    } else {
      return(TRUE)  # early exit in the case there is only a single new point
    }
  } else {
    map(seq_len(nrow(xdt)), function(i) {
      gower::gower_dist(previous_xdt, xdt[i, ], nthread = 1L)  # NOTE: no parallel execution for now
    })
  }
  map_lgl(gower_distance, function(x) all(x > distance_epsilon))  # FIXME: this is not numerically stable
}
