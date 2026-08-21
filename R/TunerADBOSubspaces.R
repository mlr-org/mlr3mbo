#' @title TunerAsync using Asynchronous Decentralized Bayesian Optimization on Homogeneous Subspaces
#'
#' @include OptimizerADBOSubspaces.R
#' @name mlr_tuners_adbo_subspaces
#'
#' @description
#' `TunerADBOSubspaces` class that implements Asynchronous Decentralized Bayesian Optimization (ADBO) on a partition
#' of the search space into homogeneous subspaces.
#' Every worker is permanently assigned to one subspace and fits its own [SurrogateLearner] on the evaluations of
#' that subspace only.
#' This is a minimal interface internally passing on to [OptimizerADBOSubspaces].
#' For additional information and documentation see [OptimizerADBOSubspaces].
#'
#' `TunerADBOSubspaces` is considered an experimental feature and the API might be subject to changes.
#' Currently, only single-objective optimization is supported.
#'
#' @inheritSection mlr_optimizers_adbo_subspaces Subspaces
#' @inheritSection mlr_optimizers_adbo_subspaces Compute Profiles
#' @inheritSection mlr_optimizers_adbo_subspaces Initial Design
#' @inheritSection mlr_optimizers_adbo_subspaces Parameters
#' @inheritSection mlr_optimizers_adbo_subspaces Note
#'
#' @references
#' * `r format_bib("egele_2023")`
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rush") &
#'     requireNamespace("mlr3learners") &
#'     requireNamespace("ranger") &
#'     requireNamespace("mlr3pipelines") &
#'     requireNamespace("rpart")) {
#'
#'   if (redis_available()) {
#'
#'     library(mlr3)
#'     library(mlr3tuning)
#'     library(mlr3pipelines)
#'
#'     # a branching parameter selects the learner
#'     graph = ppl("branch", list(
#'       rpart = lrn("classif.rpart", cp = to_tune(1e-4, 1, logscale = TRUE)),
#'       featureless = lrn("classif.featureless", method = to_tune())))
#'     learner = as_learner(graph)
#'     learner$param_set$set_values(branch.selection = to_tune())
#'
#'     instance = TuningInstanceAsyncSingleCrit$new(
#'       task = tsk("wine"),
#'       learner = learner,
#'       resampling = rsmp("cv", folds = 3),
#'       measure = msr("classif.acc"),
#'       terminator = trm("evals", n_evals = 20))
#'
#'     subspaces = partition_search_space(
#'       instance$search_space,
#'       param = "branch.selection",
#'       groups = list(rpart = "rpart", featureless = "featureless"))
#'
#'     # one worker per subspace, each on its own compute profile
#'     mirai::daemons(1, .compute = "rpart")
#'     mirai::daemons(1, .compute = "featureless")
#'     rush::rush_plan(profiles = c(rpart = 1, featureless = 1), worker_type = "mirai")
#'
#'     tnr("adbo_subspaces",
#'       subspaces = subspaces,
#'       design_size = 4)$optimize(instance)
#'
#'     mirai::daemons(0, .compute = "rpart")
#'     mirai::daemons(0, .compute = "featureless")
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
TunerADBOSubspaces = R6Class(
  "TunerADBOSubspaces",
  inherit = mlr3tuning::TunerAsyncFromOptimizerAsync,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(optimizer = OptimizerADBOSubspaces$new(), man = "mlr3mbo::mlr_tuners_adbo_subspaces")
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), if (is.na(self$label)) "" else paste0(": ", self$label))
      catn(str_indent("* Parameter classes:", self$param_classes))
      catn(str_indent("* Properties:", self$properties))
      catn(str_indent("* Packages:", self$packages))
      catn(str_indent("* Surrogate:", if (is.null(self$surrogate)) "-" else self$surrogate$print_id))
      catn(str_indent("* Acquisition Function:", if (is.null(self$acq_function)) "-" else class(self$acq_function)[1L]))
      catn(str_indent(
        "* Acquisition Function Optimizer:",
        if (is.null(self$acq_optimizer)) "-" else self$acq_optimizer$print_id
      ))
      catn(str_indent(
        "* Result Assigner:",
        if (is.null(self$result_assigner)) "-" else class(self$result_assigner)[1L]
      ))
    },

    #' @description
    #' Reset the tuner.
    #' Sets the following fields to `NULL`:
    #' `surrogate`, `acq_function`, `acq_optimizer`, `result_assigner`
    #' Resets parameter values `design_size` and `design_function` to their defaults.
    reset = function() {
      private$.optimizer$reset()
    }
  ),

  active = list(
    #' @template field_surrogate
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$surrogate
      } else {
        private$.optimizer$surrogate = assert_r6(rhs, classes = "Surrogate", null.ok = TRUE)
      }
    },

    #' @template field_acq_function
    acq_function = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$acq_function
      } else {
        private$.optimizer$acq_function = assert_r6(rhs, classes = "AcqFunction", null.ok = TRUE)
      }
    },

    #' @template field_acq_optimizer
    acq_optimizer = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$acq_optimizer
      } else {
        private$.optimizer$acq_optimizer = assert_r6(rhs, classes = "AcqOptimizer", null.ok = TRUE)
      }
    },

    #' @template field_result_assigner
    result_assigner = function(rhs) {
      if (missing(rhs)) {
        private$.optimizer$result_assigner
      } else {
        private$.optimizer$result_assigner = assert_r6(rhs, classes = "ResultAssigner", null.ok = TRUE)
      }
    },

    #' @template field_param_classes
    param_classes = function(rhs) {
      assert_ro_binding(rhs)
      private$.optimizer$param_classes
    },

    #' @template field_properties
    properties = function(rhs) {
      assert_ro_binding(rhs)
      private$.optimizer$properties
    },

    #' @template field_packages
    packages = function(rhs) {
      assert_ro_binding(rhs)
      private$.optimizer$packages
    }
  )
)

#' @include aaa.R
tuners[["adbo_subspaces"]] = TunerADBOSubspaces
