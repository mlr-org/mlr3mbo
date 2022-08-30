OptimizerChain = R6Class("OptimizerChain", inherit = bbotk::Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param optimizers (list of [Optimizer]s).
    #' @param terminators (list of [Terminator]s | NULL).
    initialize = function(optimizers, terminators = rep(list(NULL), length(optimizers))) {
      assert_list(optimizers, types = "Optimizer", any.missing = FALSE)
      assert_list(terminators, types = c("Terminator", "NULL"), len = length(optimizers))

      param_sets = vector(mode = "list", length = length(optimizers))
      ids_taken = character(0L)
      # for each optimizer check whether the id of the param_set
      # (decuded from the optimizer class) is already taken;
      # if necessary postfix the id
      for (i_opt in seq_along(optimizers)) {
        opt = optimizers[[i_opt]]
        ps = opt$param_set$clone(deep = TRUE)
        ps$set_id = class(opt)[[1L]]
        try_postfix = 0L
        while (ps$set_id %in% ids_taken) {
          try_postfix = try_postfix + 1L
          ps$set_id = paste0(class(opt)[[1L]], "_", try_postfix)
        }
        ids_taken[[i_opt]] = ps$set_id
        param_sets[[i_opt]] = ps
      }
      private$.ids = map_chr(param_sets, "set_id")
      super$initialize(
        param_set = ParamSetCollection$new(param_sets),
        param_classes = Reduce(intersect, mlr3misc::map(optimizers, "param_classes")),
        properties = Reduce(intersect, mlr3misc::map(optimizers, "properties")),
        packages = unique(unlist(mlr3misc::map(optimizers, "packages")))
      )
      private$.optimizers = optimizers
      private$.terminators = terminators
    }
  ),

  private = list(
    .optimizers = NULL,
    .terminators = NULL,
    .ids = NULL,

    .optimize = function(inst) {
      terminator = inst$terminator
      on.exit({inst$terminator = terminator})
      inner_inst = inst$clone(deep = TRUE)

      for (i_opt in seq_along(private$.optimizers)) {
        inner_term = private$.terminators[[i_opt]]
        if (!is.null(inner_term)) {
          inner_inst$terminator = TerminatorCombo$new(list(inner_term, terminator))
        } else {
          inner_inst$terminator = terminator
        }
        opt = private$.optimizers[[i_opt]]
        opt$param_set$values = self$param_set$.__enclos_env__$private$.sets[[i_opt]]$values
        opt$optimize(inner_inst)
        inner_inst$archive$data$batch_nr = max(inst$archive$data$batch_nr, 0L) +
          inner_inst$archive$data$batch_nr
        inner_inst$archive$data$optimizer = private$.ids[i_opt]
        inst$archive$data = rbind(inst$archive$data, inner_inst$archive$data, fill = TRUE)
        inner_inst$archive$data = data.table()
        if (terminator$is_terminated(inst$archive)) {
          break
        }
      }
    },

    deep_clone = function(name, value) {
      switch(
        name,
        .optimizers = mlr3misc::map(value, .f = function(x) x$clone(deep = TRUE)),
        .terminators = mlr3misc::map(value, .f = function(x) if (!is.null(x)) x$clone(deep = TRUE)),
        value
      )
    }
  )
)

