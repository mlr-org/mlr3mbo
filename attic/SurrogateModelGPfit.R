# Optimization Surrogate Model

# *points - data.table with column x and y (always!)
#

SurrogateModelGPfit = R6Class(
  "SurrogateModelGPfit",
  inherit = SurrogateModel,

  public = list(
    # public member

    # constructor
    initialize = function(dots = list(), design = data.table()) {
      super$initialize(id = "GPfit", parameters = list(dots = dots), packages = "GPfit", design = design)
    },

    # public methods

    add = function(points) {
      self$storage$model = NULL #invalidate the model!
      super$add(points)
    },

    predict = function(points) {
      # GP_fit only accepts input values [0,1], therefore we have to scale
      range = search_space$upper - search_space$lower #precalc for fastness, maybe even further 'up'?
      lower = search_space$lower
      npar = length(range)
      mat_scale = function(x) {
        mat = as.matrix(rbindlist(x)) # Suprisingly slow according to profiling
        mat = sweep(mat, 2, lower) %*% diag(1/(range), npar, npar)
      }
      if (is.null(self$storage$model)) {
        model = GPfit::GP_fit(X = mat_scale(self$design$x), Y = self$design$y)
        self$storage$model = model
      }
      # and also scale back
      res = predict(self$storage$model, mat_scale(points$x))
      data.table(mean = res$Y_hat, se = res$MSE)
    }

  ))
