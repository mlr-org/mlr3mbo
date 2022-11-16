#' @title Dictionary of Loop Functions
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class `loop_function`.
#' Each loop function has an associated help page, see `mlr_loop_functions_[id]`.
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family Loop Function
#' @export
#' @examples
#' as.data.table(mlr_loop_functions)
mlr_loop_functions = R6Class("DictionaryLoopFunction", inherit = Dictionary, cloneable = FALSE,
  public = list(
    #' @description
    #' Retrieves object with key `key` from the dictionary.
    #' Additional arguments must be named and are passed to the constructor of the stored object.
    #'
    #' @param key (`character(1)`).
    #'
    #' @param ... (`any`)\cr
    #' Passed down to constructor.
    #'
    #' @return Object with corresponding key.
    get = function(key, ...) {
      assert_string(key, min.chars = 1L)
      dictionary_loop_function_get(self, key, ...)
    }
  )
)$new()

dictionary_loop_function_get = function(self, key, ...) {
  obj = dictionary_loop_function_retrieve_item(self, key)
  obj$value
}

dictionary_loop_function_retrieve_item = function(self, key) {
  obj = get0(key, envir = self$items, inherits = FALSE, ifnotfound = NULL)
  if (is.null(obj)) {
    stopf("Element with key '%s' not found in %s!%s", key, class(self)[1L], did_you_mean(key, self$keys()))
  }
  obj
}

#' @export
as.data.table.DictionaryLoopFunction = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    lpf = x$get(key)
    insert_named(
      list(key = key, label = attr(lpf, "label"), instance = attr(lpf, "instance"), man = attr(lpf, "man")),
      if (objects) list(object = list(lpf))
    )
  }, .fill = TRUE), "key")[]
}

