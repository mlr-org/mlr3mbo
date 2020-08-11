generate_acq_codomain = function(archive, id, direction = "same") {
  if (archive$codomain$length > 1) {
    stop("not supportet yet") #FIXME: But should be
  }
  assert_choice(direction, c("same", "minimize", "maximize"))
  codomain = ParamSet$new(list(
    ParamDbl$new(id, tags = ifelse(direction == "same", archive$codomain$params[[1]]$tags, direction))
  ))
  return(codomain)
}
