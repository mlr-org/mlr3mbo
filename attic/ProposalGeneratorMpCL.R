#' @title Multipoint Constant Liar Proposals
#' @export
#' 
ProposalGeneratorMpCL = R6Class("ProposalGeneratorMpCL",
  inherit = ProposalGenerator,

  public = list(

    acq_function = NULL,
    acq_optimizer = NULL,
    q = NULL,
    liar = NULL,

    initialize = function(acq_function, acq_optimizer, liar, q) {
      self$acq_function = assert_r6(acq_function, "AcqFunction")
      self$acq_optimizer = assert_r6(acq_optimizer, "AcqOptimizer")
      self$liar = assert_function(liar)
      self$q = assert_int(q, lower = 1)
    },
    
    #' @return data.table \cr
    #'   data.table with columns of domain$ids() and possible extras
    propose = function() {
      proposals = data.table()
      temp_archive = self$archive$clone(deep = TRUE)
      temp_acq_function = self$acq_function$clone(deep = TRUE)
      lie = data.table(self$liar(self$archive$data()[[self$archive$cols_y]]))
      colnames(lie) = self$archive$cols_y
      for (i in seq_len(self$q)) {
        xdt = self$acq_optimizer$optimize(self$acq_function)
        proposals = rbind(proposals, xdt)
        temp_archive$add_evals(xdt, transform_xdt_to_xss(xdt, temp_archive$search_space), lie)
        temp_acq_function$update(temp_archive)
      }
      return(proposals)
    },

    setup = function(archive) {
      super$setup(archive)
      self$acq_function$setup(archive)
      #self$acq_optimizer$setup()
    },

    update = function() {
      self$acq_function$update(self$archive)
      #self$acq_optimizer$update()
    }


  )
)