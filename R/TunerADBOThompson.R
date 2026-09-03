#' @title TunerAsync using Asynchronous Decentralized Bayesian Optimization with Thompson Sampling over Subspaces
#'
#' @include OptimizerADBOThompson.R
#' @name mlr_tuners_adbo_thompson
#'
#' @description
#' `TunerADBOThompson` class that implements Asynchronous Decentralized Bayesian Optimization (ADBO) on a search
#' space that is partitioned into homogeneous subspaces, typically one subspace per learner.
#' Several subspaces may share a \CRANpkg{mirai} compute profile, e.g., all learners trained on CPU cores.
#' A worker is permanently assigned to its compute profile and, in each iteration, samples one of the subspaces of
#' its profile via Thompson sampling.
#' This is a minimal interface internally passing on to [OptimizerADBOThompson].
#' For additional information and documentation see [OptimizerADBOThompson].
#'
#' `TunerADBOThompson` is considered an experimental feature and the API might be subject to changes.
#' Currently, only single-objective optimization is supported.
#'
#' @inheritSection mlr_optimizers_adbo_thompson Subspaces and Compute Profiles
#' @inheritSection mlr_optimizers_adbo_thompson Thompson Sampling
#' @inheritSection mlr_optimizers_adbo_thompson Loop
#' @inheritSection mlr_optimizers_adbo_thompson Parameters
#' @inheritSection mlr_optimizers_adbo_subspaces Initial Design
#' @inheritSection mlr_optimizers_adbo_thompson Note
#'
#' @references
#' * `r format_bib("egele_2023")`
#' * `r format_bib("thompson_1933")`
#' * `r format_bib("thornton_2013")`
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
#'       ranger = lrn("classif.ranger", mtry.ratio = to_tune(0.1, 1)),
#'       featureless = lrn("classif.featureless", method = to_tune())))
#'     learner = as_learner(graph)
#'     learner$param_set$set_values(branch.selection = to_tune())
#'
#'     instance = TuningInstanceAsyncSingleCrit$new(
#'       task = tsk("wine"),
#'       learner = learner,
#'       resampling = rsmp("cv", folds = 3),
#'       measure = msr("classif.acc"),
#'       terminator = trm("evals", n_evals = 30))
#'
#'     # one subspace per learner
#'     subspaces = partition_search_space(instance$search_space, param = "branch.selection")
#'
#'     # the tree learners share the "cpu" profile, the featureless learner runs on the "fast" profile
#'     mirai::daemons(2, .compute = "cpu")
#'     mirai::daemons(1, .compute = "fast")
#'     rush::rush_plan(profiles = c(cpu = 2, fast = 1), worker_type = "mirai")
#'
#'     tnr("adbo_thompson",
#'       subspaces = subspaces,
#'       subspace_profiles = c(rpart = "cpu", ranger = "cpu", featureless = "fast"),
#'       design_size = 4)$optimize(instance)
#'
#'     mirai::daemons(0, .compute = "cpu")
#'     mirai::daemons(0, .compute = "fast")
#'   } else {
#'     message("Redis server is not available.\nPlease set up Redis prior to running the example.")
#'   }
#' }
#' }
TunerADBOThompson = R6Class(
  "TunerADBOThompson",
  inherit = mlr3tuning::TunerAsyncFromOptimizerAsync,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(optimizer = OptimizerADBOThompson$new(), man = "mlr3mbo::mlr_tuners_adbo_thompson")
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
tuners[["adbo_thompson"]] = TunerADBOThompson
