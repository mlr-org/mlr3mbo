#' @title Proposal Generator Base Class
#' @export
ProposalGenerator = R6Class("ProposalGenerator",
  public = list(

    # acq_function, acq_optimizer are no member because we can have proposal generators with multiple acq_functions or none at all

    archive = NULL,
    
    #' @return data.table \cr
    #'   data.table with columns of domain$ids() and possible extras
    propose = function() {
      stop("abstract")
    },

    setup = function(archive) {
      self$archive = assert_r6(archive, "Archive")
    },

    update = function() {
      stop("abstract")
    }


  )
)