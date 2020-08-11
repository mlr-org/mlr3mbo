generate_acq_codomain = function(archive, direction = "same") {
  if (codomain$length > 1) {
    stop("not supportet yet") #FIXME: But should be
  }
  assert_choice(direction, c("same", "min", "max"))
  codomain = archive$codomain$clone(deep = TRUE)
  codomain$params[[1]]$id = self$id
  names(codomain$params)[[1]] = self$id
  tags = codomain$params[[1]]$tags
  if (direction == "min") {
    codomain$params[[1]]$tags = union(setdiff(tags, "maximize"), "minimize")
  } else if (direction == "max") {
    codomain$params[[1]]$tags = union(setdiff(tags, "minimize"), "maximize")
  }
  return(codomain)
}
