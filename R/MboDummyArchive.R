#' @title Logging object for objective function evaluations
#'
#' @description
#' Container around a [data.table::data.table()] which stores all performed
#' function calls of the Objective.
#'
#' @section Technical details:
#'
#' The data is stored in a private `.data` field that contains a
#' [data.table::data.table()] which logs all performed function calls of the [Objective].
#' This [data.table::data.table()] is accessed with the public `$data()` method. New
#' values can be added with the `$add_evals()` method. This however is usually
#' done through the evaluation of the [OptimInstance] by the [Optimizer].
#'
#' @template param_codomain
#' @template param_search_space
#' @export

MboDummyArchive = R6Class("MboArchive",
  inherit = Archive,

  public = list(

    #' @field archive ([bbotk::Archive])
    archive = NULL, # unfortunately we can not inherit from an Object :(


    #' @field codomain ([paradox::ParamSet])\cr
    #' Codomain of objective function.
    codomain = NULL,

    #' @field search_space ([paradox::ParamSet])\cr
    #' Search space of objective.
    search_space = NULL,

    #' @field added_rows ([data.table::data.table()]).
    added_rows = data.table(),

    #' @field added_cols ([data.table::data.table()]).
    added_cols = NULL, # cbind does not work with empty data.table()

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param archive ([bbotk::Archive]).
    #' @param codomain ([paradox::ParamSet])
    #' @param search_space ([paradox::ParamSet])
    initialize = function(archive, codomain = archive$codomain, search_space = archive$search_space) {
      self$archive = archive
      self$codomain = codomain
      self$search_space = search_space
    },

    #' @description
    #' Clear archive.
    clear = function() {
      self$added_rows = data.table()
      self$added_cols = NULL
    },

    #' @description
    #' Adds function evaluations to the archive table.
    #'
    #' @param xdt ([data.table::data.table()].
    #' @param ydt ([data.table::data.table()].
    #' @param xss_trafoed (`list()`)\cr
    #' Transformed point(s) in the *domain space*
    add_evals = function(xdt, xss_trafoed = NULL, ydt) {
      assert_data_table(xdt)
      assert_data_table(ydt)

      xydt = cbind(xdt, ydt)
      if (!is.null(xss_trafoed)) {
        xydt[, "x_domain" := list(xss_trafoed)]
      }
      xydt[, "batch_nr" := self$n_batch + 1]

      self$added_rows = rbindlist(list(
        self$added_rows,
        xydt),
        fill = TRUE, use.names = TRUE
      )
    },

    #' @description
    #' Add columns.
    #'
    #' @param dt ([data.table::data.table()])
    add_cols = function(dt) {
      assert_data_table(dt, nrows = self$n_evals)
      if (is.null(self$added_cols)) {
        self$added_cols = dt
      } else (
        self$added_cols = cbind(self$added_cols, dt)
      )
    },

    #' @description
    #' Returns the best scoring evaluation. For single-crit optimization,
    #' the solution that minimizes / maximizes the objective function.
    #' For multi-crit optimization, the Pareto set / front.
    #'
    #' @param m (`integer()`)\cr
    #' Take only batches `m` into account. Default is all batches.
    #'
    #' @return [data.table::data.table()].
    best = function(m = NULL) {
      if (self$n_batch == 0L) {
        stop("No results stored in archive")
      }

      m = if (is.null(m)) {
        seq_len(self$n_batch)
      } else {
        assert_integerish(m, lower = 1L, upper = self$n_batch, coerce = TRUE)
      }

      tab = self$data()[batch_nr %in% m]

      if (self$codomain$length == 1L) {
        order = if (self$codomain$tags[1L] == "minimize") 1L else -1L
        setorderv(tab, self$codomain$ids(), order = order, na.last = TRUE)
        res = tab[1, ]
      } else {
        ymat = t(as.matrix(tab[, self$cols_y, with = FALSE]))
        minimize = map_lgl(self$codomain$tags, has_element, "minimize")
        ymat = ifelse(minimize, 1L, -1L) * ymat
        res = tab[!is_dominated(ymat)]
      }

      return(res)
    },

    #' @description
    #' Returns a [data.table::data.table()] which contains all performed
    #' [Objective] function calls.
    #'
    #' @param unnest (`character()`)\cr
    #' Set of column names for columns to unnest via [mlr3misc::unnest()].
    #' Unnested columns are stored in separate columns instead of list-columns.
    #'
    #' @return [data.table::data.table()].
    data = function(unnest = NULL) {
      data = self$archive$data(unnest)
      data = rbindlist(list(data, self$added_rows), fill = TRUE, use.names = TRUE)
      cbind(data, self$added_cols)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
    }
  ),

  active = list(

    #' @field n_evals (`integer(1)`)\cr
    #' Number of evaluations stored in the archive.
    n_evals = function() {
      self$archive$n_evals + nrow(self$added_rows)
    },

    #' @field n_batch (`integer(1)`)\cr
    #' Number of batches stored in the archive.
    n_batch = function() {
      if (nrow(self$added_rows) == 0) {
        self$archive$n_batch
      } else {
        max(self$added_rows$batch_nr)
      }
    }
  )
)
