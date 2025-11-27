#' @title Syntactic Sugar Surrogate Construction
#'
#' @description
#' This function allows to construct a [SurrogateLearner] or [SurrogateLearnerCollection] in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#'
#' If the `archive` references more than one target variable or `cols_y` contains more than one
#' target variable but only a single `learner` is specified, this learner is replicated as many
#' times as needed to build the [SurrogateLearnerCollection].
#'
#' @param learner ([mlr3::LearnerRegr] | List of [mlr3::LearnerRegr])\cr
#'   [mlr3::LearnerRegr] that is to be used within the [SurrogateLearner] or a list of [mlr3::LearnerRegr] that are to
#'   be used within the [SurrogateLearnerCollection].
#' @param input_trafo (`NULL` | [InputTrafo])\cr
#'   Input transformation.
#'   Can also be `NULL`.'
#' @param output_trafo (`NULL` | [OutputTrafo])\cr
#'   Output transformation.
#'   Can also be `NULL`.
#' @param archive (`NULL` | [bbotk::Archive])\cr
#'   [bbotk::Archive] of the [bbotk::OptimInstance] used.
#'   Can also be `NULL`.
#' @param cols_x (`NULL` | `character()`)\cr
#'   Column ids in the [bbotk::Archive] that should be used as features.
#'   Can also be `NULL` in which case this is automatically inferred based on the archive.
#' @param cols_y (`NULL` | `character()`)\cr
#'   Column id(s) in the [bbotk::Archive] that should be used as a target.
#'   If a list of [mlr3::LearnerRegr] is provided as the `learner` argument and `cols_y` is
#'   specified as well, as many column names as learners must be provided.
#'   Can also be `NULL` in which case this is automatically inferred based on the archive.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet].
#'
#' @return [SurrogateLearner] | [SurrogateLearnerCollection]
#'
#' @export
#' @examples
#' library(mlr3)
#' srlrn(lrn("regr.featureless"), catch_errors = FALSE)
#' srlrn(list(lrn("regr.featureless"), lrn("regr.featureless")))
#' @export
srlrn = function(learner, input_trafo = NULL, output_trafo = NULL, archive = NULL, cols_x = NULL, cols_y = NULL, ...) {
  dots = list(...)
  assert_learner_surrogate(learner)

  surrogate = if (test_r6(learner, classes = "Learner")) {
    SurrogateLearner$new(learner = learner, input_trafo = input_trafo, output_trafo = output_trafo, archive = archive, cols_x = cols_x, col_y = cols_y)
  } else if (inherits(learner, what = "list")) {
    if (length(learner) == 1L) {
      learner = learner[1L]
      # if a single learner is provided in a list, we unlist it
      SurrogateLearner$new(learner = learner, input_trafo = input_trafo, output_trafo = output_trafo, archive = archive, cols_x = cols_x, col_y = cols_y)
    } else {
      assert_character(cols_y, len = length(learner), null.ok = TRUE)
      SurrogateLearnerCollection$new(learners = learner, input_trafo = input_trafo, output_trafo = output_trafo, archive = archive, cols_x = cols_x, cols_y = cols_y)
    }
  }
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

#' @title Syntactic Sugar Acquisition Functions Construction
#'
#' @description
#' This function complements [mlr_acqfunctions] with functions in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#'
#' @param .keys (`character()`)\cr
#' Keys passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve
#' multiple objects.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the
#' [paradox::ParamSet], or to be set as public field. See
#' [mlr3misc::dictionary_sugar_get()] for more details.
#'
#' @return List of [AcqFunction]s
#'
#' @export
#' @examples
#' acqfs(c("ei", "pi", "cb"))
#' @export
acqfs = function(.keys, ...) {
  dictionary_sugar_mget(dict = mlr_acqfunctions, .keys, ...)
}


#' @title Syntactic Sugar Acquisition Function Optimizer Construction
#'
#' @description
#' This function allows to construct an [AcqOptimizer] in the spirit
#' of `mlr_sugar` from \CRANpkg{mlr3}.
#' @param optimizer ([bbotk::OptimizerBatch] | `character(1)`)\cr
#'   [bbotk::OptimizerBatch] that is to be used or the name of the optimizer in [mlr_acqoptimizers].
#' @param terminator ([bbotk::Terminator])\cr
#'   [bbotk::Terminator] that is to be used.
#' @param acq_function (`NULL` | [AcqFunction])\cr
#'   [AcqFunction] that is to be used.
#'   Can also be `NULL`.
#' @param callbacks (`NULL` | list of [mlr3misc::Callback])
#'   Callbacks used during acquisition function optimization.
#' @param ... (named `list()`)\cr
#' Named arguments passed to the constructor, to be set as parameters in the [paradox::ParamSet].
#'
#' @return [AcqOptimizer]
#'
#' @export
#' @examples
#' library(bbotk)
#' acqo(opt("random_search"), trm("evals"), catch_errors = FALSE)
#' @export
acqo = function(optimizer, terminator, acq_function = NULL, callbacks = NULL, ...) {
  dots = list(...)

  if (is.character(optimizer)) {
    return(dictionary_sugar_get(mlr_acqoptimizers, optimizer, ...))
  }

  acqopt = AcqOptimizer$new(optimizer = optimizer, terminator = terminator, acq_function = acq_function, callbacks = callbacks)
  acqopt$param_set$values = insert_named(acqopt$param_set$values, dots)
  acqopt
}

#' @title Syntactic Sugar Result Assigner Construction
#'
#' @description
#' This function complements [mlr_result_assigners] with functions in the spirit
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
#' @return [ResultAssigner]
#'
#' @export
#' @examples
#' ras("archive")
#' @export
ras = function(.key, ...) {
  dictionary_sugar_get(mlr_result_assigners, .key, ...)
}

#' @title Syntactic Sugar Output Trafo Construction
#'
#' @description
#' This function complements [mlr_output_trafos] with functions in the spirit
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
#' @return [OutputTrafo]
#'
#' @export
#' @examples
#' ot("standardize")
#' @export
ot = function(.key, ...) {
  dictionary_sugar_get(mlr_output_trafos, .key, ...)
}

#' @title Syntactic Sugar Input Trafo Construction
#'
#' @description
#' This function complements [mlr_input_trafos] with functions in the spirit
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
#' @return [InputTrafo]
#'
#' @export
#' @examples
#' it("unitcube")
#' @export
it = function(.key, ...) {
  dictionary_sugar_get(mlr_input_trafos, .key, ...)
}

