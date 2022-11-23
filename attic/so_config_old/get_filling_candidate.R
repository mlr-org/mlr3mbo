get_filling_candidate = function(instance) {
  newdata = generate_design_random(instance$search_space, n = 10000L)$data
  gw_dists = get_gower_dist(fct_to_char(newdata), instance$archive$data[, instance$archive$cols_x, with = FALSE])  # 0 if identical, 1 if maximally dissimilar
  min_gw_dists = apply(gw_dists, MARGIN = 1L, FUN = min)  # get the minium for each new point to the points in the archive
  which.max(min_gw_dists)
  newdata[which.max(min_gw_dists), ]
}

