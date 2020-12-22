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
  archive$data[, c(archive$cols_x, archive$cols_y), with = FALSE]
}

archive_x = function(archive) {
  archive$data()[, archive$cols_x, with = FALSE]
}

get_gower_dist = function(x, y = NULL) {
  # if y is NULL we get the Gower distance of x pairwaise with itself and set the diagonal to ones
  # otherwise we get the Gower dist_threshold of x pairwise with y
  # NOTE: no parallel execution of gower::gower_dist for now
  # NOTE: catch the skipping variable warning is useful
  if (is.null(y)) {
    gower_distance = do.call(rbind, map(seq_len(nrow(x)), function(i) {
      withCallingHandlers(gower::gower_dist(x[i, ], x, nthread = 1L),
        warning = function(warning_condition) {
          if (grepl("skipping variable with zero or non-finite range", x = warning_condition$message)) {
            invokeRestart("muffleWarning")
          }
        }
      )
    }))
    diag(gower_distance) = 1
    gower_distance
  } else {
    do.call(rbind, map(seq_len(nrow(x)), function(i) {
      withCallingHandlers(gower::gower_dist(x[i, ], y, nthread = 1L),
        warning = function(warning_condition) {
          if (grepl("skipping variable with zero or non-finite range", x = warning_condition$message)) {
            invokeRestart("muffleWarning")
          }
        }
      )
    }))
  }
}

check_gower_dist = function(gower_distance, dist_threshold) {
  rowSums(gower_distance > dist_threshold) == ncol(gower_distance)
}

get_xdt = function(acq_optimizer, acq_function, previous_xdt, search_space) {
  acq_function$surrogate$assert_insample_performance
  xdt = acq_optimizer$optimize(acq_function)
  acq_optimizer$xdt_fix_dist(xdt, previous_xdt = previous_xdt, search_space = search_space)
}
