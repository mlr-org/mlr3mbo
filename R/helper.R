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
  if (is.null(feature_types)) {
    feature_types = c("logical", "integer", "numeric", "factor")
  }
  param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct")
  param_classes[c("logical", "integer", "numeric", "factor") %in% feature_types]
}

archive_xy = function(archive) {
  archive$data[, c(archive$cols_x, archive$cols_y), with = FALSE]
}

char_to_fct = function(xydt) {
  # Convert character params to factors
  chr_cols = names(xydt)[map_chr(xydt, class) == "character"]
  if (length(chr_cols))
    xydt[, (chr_cols) := map(.SD, as.factor), .SDcols = chr_cols]
  return(xydt)
}

archive_x = function(archive) {
  archive$data[, archive$cols_x, with = FALSE]
}

get_gower_dist = function(x, y = NULL) {
  # if y is NULL we get the Gower distance of x pairwise with itself and set the diagonal to ones
  # otherwise we get the Gower dist_threshold of x pairwise with y
  # NOTE: no parallel execution of gower::gower_dist for now
  # NOTE: catching the skipping variable warning is useful
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

fix_xdt_distance = function(xdt, previous_xdt, search_space, dist_threshold) {
  # first, we check wether the Gower distance of each newly proposed point to all other newly proposed points is larger than dist_threshold
  gower_distance_new = get_gower_dist(xdt)
  check_passed_new = check_gower_dist(gower_distance_new, dist_threshold = dist_threshold)

  # second, we check wether the Gower distance of all newly proposed points to the previously evaluated ones is larger than dist_threshold
  check_passed_previous = check_gower_dist(get_gower_dist(xdt, y = previous_xdt), dist_threshold = dist_threshold)

  # if either fails we iteratively replace problematic points with randomly sampled ones until both checks pass
  if (any(!(check_passed_new & check_passed_previous))) {
    lg$info("Replacing proposed point(s) by randomly sampled ones due to low Gower distances")  # FIXME: logging?
  }

  # first, replace the ones that are too close to the previous ones
  if (any(!check_passed_previous)) {
    xdt[!check_passed_previous, ] = SamplerUnif$new(search_space)$sample(sum(!check_passed_previous))$data  # FIXME: also think about augmented lhs
  }

  # second, replace the ones that are too close to the other new ones
  # do this iteratively starting with the lowest distance because the distances change once a single point is replaced and we do not want to replace too many
  # this could change the results of the first check but we don't care, see comment below
  if (any(!check_passed_new)) {
    # this does not necessarily imply that the second check eventually passes but again we don't care, see comment below
    for (i in seq_len(sum(!check_passed_new))) {
      xdt[arrayInd(which.min(gower_distance_new), dim(gower_distance_new))[, 1L], ] = SamplerUnif$new(search_space)$sample(1L)$data  # FIXME: also think about augmented lhs
      gower_distance_new = get_gower_dist(xdt)
      if (all(check_gower_dist(gower_distance_new, dist_threshold = dist_threshold))) {
        break  # early exit as soon as the distances pass
      }
    }
  }

  # NOTE: the cleanest way to do this would be a while loop so we can be sure that both checks eventually pass but this is too costly

  xdt
}

