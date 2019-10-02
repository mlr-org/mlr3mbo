# Optimization Surrogate Model

# *points - data.table with column x and y (always!)
#

SurrogateModel = R6Class(
  "SurrogateModel",

  public = list(
    # public member
    id = character(),
    name = character(),
    storage = list(),
    parameters = list(),
    packages = character(),
    search_space = NULL,
    design = data.table(),

    # constructor
    initialize = function(id, name = id, parameters, packages, design = data.table()) {
      self$id = id
      self$name = name
      self$parameters = parameters
      self$packages = packages
      self$design = design
    },

    # public methods

    prepare = function(design, search_space) {
      self$design = design
      self$search_space = search_space
    },

    # add points (x,y) to the surrogate
    # points: data.table with columns (x, y)
    add = function(points) {
      self$design = rbind(self$design, points)
    },

    # remove points from the surrogate (optional)
    remove = function(points) {
      stop("Not implemented.")
    },

    # replace old_points by new_points (optional)
    update = function(old_points, new_points) {
      stop("Not implemented.")
    },

    # obtain mean response (and optional se)
    # returns data.table with columns mean (+se)
    predict = function(points) {
      stop("Not implemented.")
    },

    # returns data.table with columns mean (+se) for choosen resolution within a grid of the search_space
    surface = function(resolution = 10) {
      points = paradox::generate_design_grid(param_set = self$search_space, resolution = resolution)
      self$predict(points)
    }

  ))
