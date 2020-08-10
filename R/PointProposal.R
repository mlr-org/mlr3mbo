PointProposal = R6Class("PointProposal",
  public = list(

    archive = NULL,
    
    #' @value data.table \cr
    #'   data.table with columns of domain$ids() and possible extras
    propose = function() {
      stop("abstract")
    },

    setup = function(archive) {
      self$archive = assert_r6(archive, "archive")
    },

    update = function() {
      stop("abstract")
    }


  )
)