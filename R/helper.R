generate_acq_codomain = function(codomain, id, direction = "same") {
  assert_choice(direction, c("same", "minimize", "maximize"))
  if (direction == "same") {
    if (codomain$length > 1) {
      stop("not supportet yet") #FIXME: But should be
    }
    tags = codomain$params[[1]]$tags
  } else {
    tags = direction
  }
  codomain = ParamSet$new(list(
    ParamDbl$new(id, tags = direction)
  ))
  return(codomain)
}

mult_max_to_min = function(codomain) {
  ifelse(codomain$tags == "minimize", 1, -1)
}
