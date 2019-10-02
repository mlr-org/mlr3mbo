Archive = R6Class("Archive", 
  public = list(
    data = NULL,
    param_set = NULL,

    initialize = function(param_set) {
      self$param_set = param_set
      self$data = data.table()
    },

    add = function(dt) {
      #FIXME: assert von d
      self$data = rbindlist(list(self$data, dt), fill = TRUE, use.names = TRUE)
    },
    
    print = function() {
      catf("Archive:")
      print(self$data)
    }
  ),
  
  active = list(
    n_evals = function() nrow(self$data),
    idx_unevaled = function() self$data$y
  ),
)

