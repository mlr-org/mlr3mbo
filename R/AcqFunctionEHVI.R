#' @title Acquisition Function Expected Hypervolume Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ehvi
#'
#' @description
#' Expected Hypervolume Improvement.
#'
#' @section Parameters:
#' * `"n_mc"` (`integer(1)`)\cr
#'   Number of MC samples to approximate the expectation.
#'
#' @family Acquisition Function
#' @export
AcqFunctionEHVI = R6Class("AcqFunctionEHVI",
  inherit = AcqFunction,

  public = list(

    #' @field ref_point (`numeric()`).
    ref_point = NULL,

    #' @field hypervolume (`numeric(1)`).
    hypervolume = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearners]).
    #' @param n_mc (`integer(1)`).
    initialize = function(surrogate = NULL, n_mc = 100L) {
      if (!requireNamespace("emoa", quietly = TRUE)) {
        stop("The 'emoa' package is required for AcqFunctionEHVI.")
      }
      if (!requireNamespace("mvtnorm", quietly = TRUE)) {
        stop("The 'mvtnorm' package is required for AcqFunctionEHVI.")
      }
      assert_r6(surrogate, "SurrogateLearners", null.ok = TRUE)
      assert_int(n_mc, lower = 2L)

      constants = ParamSet$new(list(
        ParamInt$new("n_mc", lower = 2L, default = 100L)
      ))
      constants$values$n_mc = n_mc

      super$initialize("acq_ehvi", constants = constants, surrogate = surrogate, direction = "maximize")
    },

    #' @description
    #' Updates acquisition function and sets `ys_front`, `ref_point`, `epsilon`.
    update = function() {
      super$update()
      ys = self$archive$data[, self$archive$cols_y, with = FALSE]
      ys = as.matrix(ys) %*% diag(self$surrogate_max_to_min)
      ref_point = apply(ys, MARGIN = 2L, FUN = max) + 1  # offset = 1 like in mlrMBO
      self$ref_point = ref_point
      self$hypervolume = emoa::dominated_hypervolume(t(ys), ref = t(ref_point))
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      # FIXME: this is just brute force MC integration
      # should also implement Yang et al. (2019) or Daulton et al. (2020)
      # FIXME: check that n_mc is in ... and use this
      if (is.null(self$ref_point)) {
        stop("ref_point is not set. Missed to call $update()?")
      }
      if (is.null(self$hypervolume)) {
        stop("hypervolume is not set. Missed to call $update()?")
      }
      ps = self$surrogate$predict(xdt)
      means = as.matrix(map_dtc(ps, "mean"))
      vars = as.matrix(map_dtc(ps, "se")) ^ 2
      ys = self$archive$data[, self$archive$cols_y, with = FALSE]
      ys = as.matrix(ys) %*% diag(self$surrogate_max_to_min)
      n_mc = self$constants$values$n_mc

      ehvi = map_dbl(seq_len(nrow(xdt)), function(i) {
        hvis = map_dbl(seq_len(n_mc), function(j) {
          y = mvtnorm::rmvnorm(1L, mean = means[i, ], sigma = diag(vars[i, ]))
          ys_tmp = rbind(ys, y)
          hvi = emoa::dominated_hypervolume(t(ys_tmp), ref = t(self$ref_point)) - self$hypervolume
          hvi
        })

        mean(hvis)
      })

      ehvi[ehvi < 0] = 0

      data.table(acq_ehvi = ehvi)
    }
  )
)

mlr_acqfunctions$add("ehvi", AcqFunctionEHVI)
