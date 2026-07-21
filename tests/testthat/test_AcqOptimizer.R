test_that("AcqOptimizer API works", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")

  # EI, random search
  instance = OptimInstanceBatchSingleCrit$new(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)
  acqfun = AcqFunctionEI$new(SurrogateLearner$new(REGR_KM_DETERM, archive = instance$archive))
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L), acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()
  expect_data_table(acqopt$optimize(), nrows = 1L)

  # upgrading error class works - catch_errors
  acqopt = AcqOptimizer$new(OptimizerError$new(), trm("evals", n_evals = 2L), acq_function = acqfun)
  expect_error(acqopt$optimize(), class = "Mlr3ErrorMboAcqOptimizer")

  acqopt$param_set$values$catch_errors = FALSE
  expect_error(acqopt$optimize(), class = "simpleError")

  # logging_level
  lg_bbotk = lgr::get_logger("mlr3/bbotk")
  console_appender = if (packageVersion("lgr") >= "0.4.0") {
    lg_bbotk$inherited_appenders$console
  } else {
    lg_bbotk$inherited_appenders$appenders.console
  }
  f = tempfile("bbotklog_", fileext = "log")
  th1 = lg_bbotk$threshold
  th2 = console_appender$threshold

  lg_bbotk$set_threshold("debug")
  lg_bbotk$add_appender(lgr::AppenderFile$new(f, threshold = "debug"), name = "testappender")
  console_appender$set_threshold("warn")

  on.exit({
    lg_bbotk$remove_appender("testappender")
    lg_bbotk$set_threshold(th1)
    console_appender$set_threshold(th2)
  })

  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L), acq_function = acqfun)
  acqopt$param_set$values$logging_level = "warn"
  acqopt$optimize()
  lines = readLines(f)
  expect_equal(lines, character(0))

  acqopt$param_set$values$logging_level = "info"
  acqopt$optimize()
  lines = readLines(f)
  expect_character(lines, min.len = 1L)

  # n_candidates | warmstart | warmstart_size | skip_already_evaluated
  acqopt = AcqOptimizer$new(
    opt("design_points", batch_size = 1L, design = data.table(x = c(-1, -0.5, 0, 0.5, 1))),
    trm("evals", n_evals = 5L),
    acq_function = acqfun
  )
  acqopt$param_set$values$n_candidates = 3L
  xdt = acqopt$optimize()
  expect_true(nrow(xdt) == 3L)
  expect_setequal(xdt[["x"]], c(-0.5, 0, 0.5))

  acqopt = AcqOptimizer$new(
    opt("design_points", batch_size = 1L, design = data.table(x = 0)),
    trm("evals", n_evals = 5L),
    acq_function = acqfun
  )
  acqopt$param_set$values$warmstart = TRUE
  xdt = acqopt$optimize()
  expect_true(xdt[["x"]] == 0)
  expect_false(xdt[[".already_evaluated"]])

  acqopt$param_set$values$warmstart_size = 1L
  xdt = acqopt$optimize()
  expect_true(xdt[["x"]] == 0)
  expect_false(xdt[[".already_evaluated"]])

  acqopt = AcqOptimizer$new(
    opt("grid_search", resolution = 4L, batch_size = 1L),
    trm("evals", n_evals = 8L),
    acq_function = acqfun
  )
  acqopt$param_set$values$warmstart = TRUE
  acqopt$param_set$values$warmstart_size = "all"
  expect_error(acqopt$optimize(), regexp = "Less then", class = "Mlr3ErrorMboAcqOptimizer")

  acqopt$param_set$values$skip_already_evaluated = FALSE
  xdt = acqopt$optimize()
  expect_true((xdt[[".already_evaluated"]]))

  acqopt$param_set$values$warmstart_size = NULL
  acqopt$param_set$values$warmstart = FALSE
  xdt = acqopt$optimize()
  expect_true((xdt[[".already_evaluated"]]))
})

test_that("AcqOptimizer param_set", {
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 1L), trm("evals", n_evals = 1L))
  expect_r6(acqopt$param_set, "ParamSet")
  expect_setequal(
    acqopt$param_set$ids(),
    c("n_candidates", "logging_level", "warmstart", "warmstart_size", "skip_already_evaluated", "catch_errors")
  )
  expect_equal(acqopt$param_set$class[["n_candidates"]], "ParamInt")
  expect_equal(acqopt$param_set$class[["logging_level"]], "ParamFct")
  expect_equal(acqopt$param_set$class[["warmstart"]], "ParamLgl")
  expect_equal(acqopt$param_set$class[["warmstart_size"]], "ParamInt")
  expect_equal(acqopt$param_set$class[["skip_already_evaluated"]], "ParamLgl")
  expect_equal(acqopt$param_set$class[["catch_errors"]], "ParamLgl")
  expect_error(
    {
      acqopt$param_set = list()
    },
    regexp = "param_set is read-only."
  )
})

test_that("AcqOptimizer trafo", {
  domain = ps(x = p_dbl(lower = 10, upper = 20, trafo = function(x) x - 15))
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = xdt$x^2),
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize")),
    check_values = FALSE
  )
  instance = MAKE_INST(objective = objective, search_space = domain, terminator = trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)
  acqfun = AcqFunctionEI$new(SurrogateLearner$new(REGR_FEATURELESS, archive = instance$archive))
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 2L), trm("evals", n_evals = 2L), acq_function = acqfun)
  acqfun$surrogate$update()
  acqfun$update()
  res = acqopt$optimize()
  expect_equal(res$x, res$x_domain[[1L]][[1L]])
})

test_that("AcqOptimizer deep clone", {
  acqopt1 = AcqOptimizer$new(opt("random_search", batch_size = 1L), trm("evals", n_evals = 1L))
  acqopt2 = acqopt1$clone(deep = TRUE)
  expect_true(address(acqopt1) != address(acqopt2))
  expect_true(address(acqopt1$optimizer) != address(acqopt2$optimizer))
  expect_true(address(acqopt1$terminator) != address(acqopt2$terminator))
})

test_that("AcqOptimizer callbacks", {
  instance = OptimInstanceBatchSingleCrit$new(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = MAKE_DESIGN(instance)
  instance$eval_batch(design)
  callback = callback_batch(
    "mlr3mbo.acqopt_time",
    on_optimization_begin = function(callback, context) {
      callback$state$begin = Sys.time()
    },
    on_optimization_end = function(callback, context) {
      callback$state$end = Sys.time()
      attr(callback$state$outer_instance, "acq_opt_runtime") = as.numeric(callback$state$end - callback$state$begin)
    }
  )
  callback$state$outer_instance = instance
  acqfun = AcqFunctionEI$new(SurrogateLearner$new(REGR_FEATURELESS, archive = instance$archive))
  acqopt = AcqOptimizer$new(
    opt("random_search", batch_size = 10L),
    trm("evals", n_evals = 10L),
    acq_function = acqfun,
    callbacks = callback
  )
  acqfun$surrogate$update()
  acqfun$update()
  res = acqopt$optimize()
  expect_number(attr(instance, "acq_opt_runtime"))
})

test_that("AcqOptimizer warmstart respects warmstart_size for multi-crit archives", {
  skip_if_not_installed("emoa")

  instance = MAKE_INST(objective = OBJ_1D_2, search_space = PS_1D, terminator = trm("evals", n_evals = 20L))
  instance$eval_batch(data.table(x = seq(-1, 1, length.out = 8L)))
  surrogate = srlrn(list(lrn("regr.featureless"), lrn("regr.featureless")), archive = instance$archive)
  acqfun = acqf("smsego", surrogate = surrogate)
  acqfun$surrogate$update()
  acqfun$progress = 1
  acqfun$update()

  # the non-dominated front has more than one point, so best() would ignore warmstart_size
  expect_gt(nrow(instance$archive$best()), 1L)

  callback = callback_batch("mlr3mbo.warmstart_count",
    on_optimization_begin = function(callback, context) {
      attr(callback$state$outer, "n_warmstart") = context$instance$archive$n_evals
    }
  )
  acqopt = AcqOptimizer$new(
    opt("random_search", batch_size = 20L),
    trm("evals", n_evals = 20L),
    acq_function = acqfun,
    callbacks = callback
  )
  callback$state$outer = acqopt
  acqopt$param_set$set_values(warmstart = TRUE, warmstart_size = 1L)
  acqopt$optimize()
  expect_equal(attr(acqopt, "n_warmstart"), 1L)
})

test_that("AcqOptimizer deep cloning works", {
  # base class with optimizer and terminator
  acqopt = AcqOptimizer$new(opt("random_search", batch_size = 10L), trm("evals", n_evals = 10L))
  acqopt_clone = acqopt$clone(deep = TRUE)
  expect_class(acqopt_clone, "AcqOptimizer")
  expect_false(identical(acqopt$optimizer, acqopt_clone$optimizer))
  expect_false(identical(acqopt$terminator, acqopt_clone$terminator))
  expect_false(identical(acqopt$param_set, acqopt_clone$param_set))

  # subclasses have NULL optimizer and terminator and must still deep clone
  for (key in c("direct", "lbfgsb", "local_search", "random_search")) {
    acqopt = acqo(key)
    acqopt_clone = acqopt$clone(deep = TRUE)
    expect_class(acqopt_clone, class(acqopt)[1L])
    expect_false(identical(acqopt$param_set, acqopt_clone$param_set))
  }
})

test_that("mlr_acqoptimizers dictionary and label/man work", {
  tab = as.data.table(mlr_acqoptimizers)
  expect_data_table(tab, ncols = 3L)
  expect_names(names(tab), permutation.of = c("key", "label", "man"))
  expect_character(tab$label, any.missing = FALSE)
  expect_character(tab$man, any.missing = FALSE)

  # base AcqOptimizer derives its label from the wrapped bbotk::Optimizer
  optimizer = opt("random_search")
  acqopt = AcqOptimizer$new(optimizer, trm("evals", n_evals = 2L))
  expect_equal(acqopt$label, optimizer$label)
  expect_equal(acqopt$man, "mlr3mbo::AcqOptimizer")
  expect_error({acqopt$label = "x"}, "read-only")
  expect_error({acqopt$man = "x"}, "read-only")
})
