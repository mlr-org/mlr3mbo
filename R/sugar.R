#' @title Syntactic Sugar Surrogate Learner Construction
#'
#' @description
#' This function allows to construct a [SurrogateLearner] in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#'
#' @param learner ([mlr3::LearnerRegr])\cr
#'   [mlr3::LearnerRegr] that is to be used.
#' @param archive (`NULL` | [bbotk::Archive])\cr
#'   [bbotk::Archive] of the [bbotk::OptimInstance] used.
#'   Can also be `NULL`.
#' @param x_cols (`NULL` | `character()`)\cr
#'   Names of columns in the [bbotk::Archive] that should be used as features.
#'   Can also be `NULL`.
#' @param y_col (`NULL` | `character(1)`)\cr
#'   Name of the column in the [bbotk::Archive] that should be used as a target.
#'   Can also be `NULL`.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet].
#'
#' @return [SurrogateLearner]
#'
#' @export
#' @examples
#' srlrn(lrn("regr.featureless"), catch_errors = FALSE)
#' @export
srlrn = function(learner, archive = NULL, x_cols = NULL, y_col = NULL, ...) {
  dots = list(...)
  surrogate = SurrogateLearner$new(learner = learner, archive = archive, x_cols = x_cols, y_col = y_col)
  surrogate$param_set$values = insert_named(surrogate$param_set$values, dots)
  surrogate
}

#' @title Syntactic Sugar Surrogate Learner Collection Construction
#'
#' @description
#' This function allows to construct a [SurrogateLearnerCollection] in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#'
#' @param learners (List of [mlr3::LearnerRegr])\cr
#'   [mlr3::LearnerRegr] that are to be used.
#' @param archive (`NULL` | [bbotk::Archive])\cr
#'   [bbotk::Archive] of the [bbotk::OptimInstance] used.
#'   Can also be `NULL`.
#' @param x_cols (`NULL` | `character()`)\cr
#'   Names of columns in the [bbotk::Archive] that should be used as features.
#'   Can also be `NULL`.
#' @param y_cols (`NULL` | `character()`)\cr
#'   Names of the columns in the [bbotk::Archive] that should be used as targets.
#'   Can also be `NULL`.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet].
#'
#' @return [SurrogateLearnerCollection]
#'
#' @export
#' @examples
#' srlrnc(list(lrn("regr.featureless"), lrn("regr.featureless")), catch_errors = FALSE)
#' @export
srlrnc = function(learners, archive = NULL, x_cols = NULL, y_cols = NULL, ...) {
  dots = list(...)
  surrogate = SurrogateLearnerCollection$new(learners = learners, archive = archive, x_cols = x_cols, y_cols = y_cols)
  surrogate$param_set$values = insert_named(surrogate$param_set$values, dots)
  surrogate
}

#' @title Syntactic Sugar Acquisition Function Construction
#'
#' @description
#' This function complements [mlr_acqfunctions] with functions in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#'
#' @param .key (`character(1)`)\cr
#' Key passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve
#' the object.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet], or to be set as public field. See
#' [mlr3misc::dictionary_sugar_get()] for more details.
#'
#' @return [AcqFunction]
#'
#' @export
#' @examples
#' acqf("ei")
#' @export
acqf = function(.key, ...) {
  dictionary_sugar_get(mlr_acqfunctions, .key, ...)
}

#' @title Syntactic Sugar Acquisition Function Optimizer Construction
#'
#' @description
#' This function allows to construct an [AcqOptimizer] in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#' @param optimizer ([bbotk::Optimizer])\cr
#'   [bbotk::Optimizer] that is to be used.
#' @param terminator ([bbotk::Terminator])\cr
#'   [bbotk::Terminator] that is to be used.
#' @param acq_function (`NULL` | [AcqFunction])\cr
#'   [AcqFunction] that is to be used.
#'   Can also be `NULL`.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet].
#'
#' @return [AcqOptimizer]
#'
#' @export
#' @examples
#' library(bbotk)
#' acqo(opt("random_search"), trm("evals"), catch_errors = FALSE)
#' @export
acqo = function(optimizer, terminator, acq_function = NULL, ...) {
  dots = list(...)
  acqopt = AcqOptimizer$new(optimizer = optimizer, terminator = terminator, acq_function = acq_function)
  acqopt$param_set$values = insert_named(acqopt$param_set$values, dots)
  acqopt
}

