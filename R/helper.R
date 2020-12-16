generate_acq_codomain = function(codomain, id, direction = "same") {
  assert_choice(direction, c("same", "minimize", "maximize"))
  if (direction == "same") {
    if (codomain$length > 1) {
      stop("not supportet yet") #FIXME: But should be
    }
    tags = codomain$params[[1]]$tags
    tags = tags[tags %in% c("minimize", "maximize")] # only filter out the relevant one
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
