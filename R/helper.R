generate_acq_codomain = function(archive, id, direction = "same") {
  assert_choice(direction, c("same", "minimize", "maximize"))
  if (direction == "same") {
    if (archive$codomain$length > 1) {
      stop("not supportet yet") #FIXME: But should be
    }
    tags = archive$codomain$params[[1]]$tags
  } else {
    tags = direction
  }
  codomain = ParamSet$new(list(
    ParamDbl$new(id, tags = direction)
  ))
  return(codomain)
}
