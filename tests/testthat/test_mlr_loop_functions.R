test_that("mlr_loop_functions", {
  expect_dictionary_loop_function(mlr_loop_functions, min_items = 1L)
  keys = mlr_loop_functions$keys()

  for (key in keys) {
    l = mlr_loop_functions$get(key)
    expect_class(l, classes = "loop_function")
  }
})

test_that("as.data.table(mlr_loop_functions)", {
  d = as.data.table(mlr_loop_functions)
  expect_data_table(d)
  expect_character(d$key, unique = TRUE, any.missing = FALSE)
  expect_character(d$label, unique = TRUE, any.missing = FALSE)
  expect_character(d$man, unique = TRUE, any.missing = FALSE)
})

test_that("as.data.table(..., objects = TRUE)", {
  tab = as.data.table(mlr_loop_functions, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "loop_function", any.missing = FALSE)
})

test_that("custom loop_function", {
  loop_function = "loop_function"
  expect_error(opt("mbo", loop_function = loop_function), "Must inherit from class 'loop_function', but has class 'character'")

  class(loop_function) = "loop_function"
  expect_error(opt("mbo", loop_function = loop_function), "Must be a function, not 'loop_function'")

  loop_function = function() {
  }
  class(loop_function) = "loop_function"
  expect_error(opt("mbo", loop_function = loop_function), "Must have formal arguments: instance,surrogate,acq_function,acq_optimizer")

  loop_function = function(instance, surrogate, acq_function, acq_optimizer, test) {
  }
  class(loop_function) = "loop_function"
  expect_error(opt("mbo", loop_function = loop_function), "Attributes must include '\\{'id','label','instance','man'\\}' but is '\\{'srcref','class'\\}'")

  attr(loop_function, "id") = "test"
  attr(loop_function, "label") = "test"
  attr(loop_function, "instance") = "test"
  attr(loop_function, "man") = "test"
  expect_error(opt("mbo", loop_function = loop_function), "'instance' attribute must be a subset of '\\{'single-crit','multi-crit'\\}' but is '\\{'test'\\}")

  attr(loop_function, "instance") = "single-crit"
  optimizer = opt("mbo", loop_function = loop_function)
  expect_r6(optimizer, classes = "OptimizerMbo")

  instance = MAKE_INST_1D(terminator = trm("evals", n_evals = 10L))
  optimizer$surrogate = default_surrogate(instance, learner = REGR_FEATURELESS)

  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)

  res = optimizer$optimize(instance)
  expect_equal(res, instance$result)
  expect_data_table(instance$archive$data, any.missing = TRUE, nrows = nrow(design))

  optimizer$optimize(instance)
})

