library(mlr3)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

# Simple 1D Functions
PS_1D = ps(
  x = p_dbl(lower = -1, upper = 1)
)
FUN_1D = function(xs) {
  list(y = as.numeric(xs)^2)
}
FUN_1D_CODOMAIN = ps(y = p_dbl(tags = "minimize"))
OBJ_1D = bbotk::ObjectiveRFun$new(fun = FUN_1D, domain = PS_1D, codomain = FUN_1D_CODOMAIN, properties = "single-crit")

FUN_1D_2 = function(xs) {
  list(y1 = as.numeric(xs)^2, y2 = - sqrt(abs(as.numeric(xs))))
}
FUN_1D_2_CODOMAIN = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
OBJ_1D_2 = bbotk::ObjectiveRFun$new(fun = FUN_1D_2, domain = PS_1D, codomain = FUN_1D_2_CODOMAIN, properties = "multi-crit")

FUN_1D_MAXIMIZE = function(xs) {
  list(y = - as.numeric(xs)^2)
}
FUN_1D_CODOMAIN_MAXIMIZE = ps(y = p_dbl(tags = "maximize"))
OBJ_1D_MAXIMIZE = bbotk::ObjectiveRFun$new(fun = FUN_1D_MAXIMIZE, domain = PS_1D, codomain = FUN_1D_CODOMAIN_MAXIMIZE, properties = "single-crit")

# Simple 1D Functions with noise
FUN_1D_NOISY = function(xs) {
  list(y = as.numeric(xs)^2 + rnorm(1, sd = 0.5))
}
OBJ_1D_NOISY = bbotk::ObjectiveRFun$new(fun = FUN_1D_NOISY, domain = PS_1D, codomain = FUN_1D_CODOMAIN, properties = c("single-crit", "noisy"))

FUN_1D_2_NOISY = function(xs) {
  list(y1 = as.numeric(xs)^2 + rnorm(1, sd = 0.5), y2 = sqrt(abs(as.numeric(xs))) + rnorm(1, sd = 0.5))
}
OBJ_1D_2_NOISY = bbotk::ObjectiveRFun$new(fun = FUN_1D_2, domain = PS_1D, codomain = FUN_1D_2_CODOMAIN, properties = c("multi-crit", "noisy"))

# Mixed 1D Functions
PS_1D_MIXED = ps(
  x1 = p_dbl(-5, 5),
  x2 = p_fct(c("a", "b", "c")),
  x3 = p_int(1L, 2L),
  x4 = p_lgl()
)
PS_1D_MIXED_DEPS = PS_1D_MIXED$clone(deep = TRUE)
PS_1D_MIXED_DEPS$add_dep("x2", on = "x4", cond = CondEqual$new(TRUE))

FUN_1D_MIXED = function(xs) {
  if (is.null(xs$x2)) {
    xs$x2 = "a"
  }
  list(y = (xs$x1 - switch(xs$x2, "a" = 0, "b" = 1, "c" = 2)) %% xs$x3 + (if (xs$x4) xs$x1 else pi))
}
OBJ_1D_MIXED = bbotk::ObjectiveRFun$new(fun = FUN_1D_MIXED, domain = PS_1D_MIXED, properties = "single-crit")
OBJ_1D_MIXED_DEPS = bbotk::ObjectiveRFun$new(fun = FUN_1D_MIXED, domain = PS_1D_MIXED_DEPS, properties = "single-crit")

FUN_1D_2_MIXED = function(xs) {
  if (is.null(xs$x2)) {
    xs$x2 = "a"
  }
  list(y1 = (xs$x1 - switch(xs$x2, "a" = 0, "b" = 1, "c" = 2)) %% xs$x3 + (if (xs$x4) xs$x1 else pi), y2 = xs$x1)
}
OBJ_1D_2_MIXED = bbotk::ObjectiveRFun$new(fun = FUN_1D_2_MIXED, domain = PS_1D_MIXED, codomain = FUN_1D_2_CODOMAIN, properties = "multi-crit")

# Simple 2D Functions
PS_2D = ps(
  x1 = p_dbl(lower = -1, upper = 1),
  x2 = p_dbl(lower = -1, upper = 1)
)
PS_2D_trafo = ps(
  x1 = p_dbl(lower = -1, upper = 1),
  x2 = p_dbl(lower = -1, upper = 1, trafo = function(x) x^2)
)
FUN_2D = function(xs) {
  y = sum(as.numeric(xs)^2)
  list(y = y)
}
FUN_2D_CODOMAIN = ps(y = p_dbl(tags = c("minimize", "random_tag")))
OBJ_2D = bbotk::ObjectiveRFun$new(fun = FUN_2D, domain = PS_2D, properties = "single-crit")

# Simple 2D Function with noise
FUN_2D_NOISY = function(xs) {
  y = sum(as.numeric(xs)^2) + rnorm(1, sd = 0.5)
  list(y = y)
}
OBJ_2D_NOISY = bbotk::ObjectiveRFun$new(fun = FUN_2D_NOISY, domain = PS_2D, properties = c("single-crit", "noisy"))

# Instance helper
MAKE_INST = function(objective = OBJ_2D, search_space = PS_2D, terminator = trm("evals", n_evals = 5)) {
  if (objective$codomain$length == 1L) {
    OptimInstanceBatchSingleCrit$new(objective = objective, search_space = search_space, terminator = terminator)
  } else {
    OptimInstanceBatchMultiCrit$new(objective = objective, search_space = search_space, terminator = terminator)
  }
}

MAKE_INST_1D = function(terminator = trm("evals", n_evals = 5L)) {
  MAKE_INST(objective = OBJ_1D, search_space = PS_1D, terminator = terminator)
}

MAKE_INST_1D_NOISY = function(terminator = trm("evals", n_evals = 5L)) {
  MAKE_INST(objective = OBJ_1D_NOISY, search_space = PS_1D, terminator = terminator)
}

MAKE_DESIGN = function(instance, n = 4L) {
  generate_design_random(instance$search_space, n)$data
}

skip_if_missing_regr_km = function() {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("DiceKriging")
  skip_if_not_installed("rgenoud")
}

if (requireNamespace("mlr3learners") && requireNamespace("DiceKriging") && requireNamespace("rgenoud")) {
  library(mlr3learners)
  REGR_KM_NOISY = lrn("regr.km", covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.estim = TRUE, jitter = 1e-12)
  REGR_KM_NOISY$encapsulate("callr", lrn("regr.featureless"))
  REGR_KM_DETERM = lrn("regr.km", covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.stability = 10^-8)
  REGR_KM_DETERM$encapsulate("callr", lrn("regr.featureless"))
}
REGR_FEATURELESS = lrn("regr.featureless")
REGR_FEATURELESS$encapsulate("callr", lrn("regr.featureless"))

OptimizerError = R6::R6Class("OptimizerError",
  inherit = OptimizerBatch,
  public = list(

    initialize = function() {
      super$initialize(
        param_set = ps(),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      stop("Optimizer Error.")
    }
  )
)

LearnerRegrError = R6::R6Class("LearnerRegrError",
  inherit = LearnerRegr,
  public = list(

    initialize = function() {
      ps = ps(error_train = p_lgl(default = TRUE, tags = "train"), error_predict = p_lgl(default = TRUE, tags = "predict"))
      ps$values = list(error_train = TRUE, error_predict = TRUE)
      super$initialize(
        id = "regr.error",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = ps
      )
    }
  ),

  private = list(
    .train = function(task) {
      if (self$param_set$values$error_train) {
        stop("Surrogate Train Error.")
      } else {
        mu = mean(task$data(cols = task$target_names)[[1L]])
        sigma = sd(task$data(cols = task$target_names)[[1L]])
        list(mu = mu, sigma = sigma)
      }
    },

    .predict = function(task) {
      if (self$param_set$values$error_predict) {
        stop("Surrogate Predict Error.")
      } else {
        n = task$nrow
        if (self$predict_type == "se") {
          list(response = rep(self$model$mu, n), se = rep(self$model$sigma, n))
        } else {
          list(response = rep(self$model$mu, n), se = rep(self$model$sigma, n))
        }
      }
    }
  )
)

expect_man_exists = function(man) {
  checkmate::expect_string(man, na.ok = TRUE, fixed = "::")
  if (!is.na(man)) {
    parts = strsplit(man, "::", fixed = TRUE)[[1L]]
    matches = help.search(parts[2L], package = parts[1L], ignore.case = FALSE)
    checkmate::expect_data_frame(matches$matches, min.rows = 1L, info = "man page lookup")
  }
}

expect_dictionary_loop_function = function(d, contains = NA_character_, min_items = 0L) {
  expect_r6(d, "Dictionary")
  testthat::expect_output(print(d), "Dictionary")
  keys = d$keys()

  expect_environment(d$items)
  expect_character(keys, any.missing = FALSE, min.len = min_items, min.chars = 1L)
  if (!is.na(contains)) {
    expect_list(d$mget(keys), types = contains, names = "unique")
  }
  expect_data_table(data.table::as.data.table(d), key = "key", nrows = length(keys))
}

expect_loop_function = function(lpf) {
  expect_class(lpf, "loop_function")
  expect_subset(c("instance", "surrogate", "acq_function", "acq_optimizer", "init_design_size", "random_interleave_iter"), names(formals(lpf)),)
  expect_subset(c("id", "label", "instance", "man"), names(attributes(lpf)))
  expect_string(attr(lpf, "id"), pattern = "bayesopt")
  expect_string(attr(lpf, "label"))
  expect_choice(attr(lpf, "instance"), c("single-crit", "multi-crit"))
  expect_man_exists(attr(lpf, "man"))
}

expect_acqfunction = function(acqf) {
  expect_r6(acqf, classes = c("AcqFunction", "Objective"))
  expect_string(acqf$id, pattern = "acq")
  expect_string(acqf$label)
  expect_man_exists(acqf$man)
}

flush_redis = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
}

expect_rush_reset = function(rush, type = "kill") {
  rush$reset(type = type)
  Sys.sleep(1)
  keys = rush$connector$command(c("KEYS", "*"))
  if (!test_list(keys, len = 0)) {
    stopf("Found keys in redis after reset: %s", keys)
  }
  mirai::daemons(0)
}

sortnames = function(x) {
  if (!is.null(names(x))) {
    x = x[order(names(x), decreasing = TRUE)]
  }
  x
}

expect_equal_sorted = function(x, y, ...) {
  expect_equal(sortnames(x), sortnames(y), ...)
}

