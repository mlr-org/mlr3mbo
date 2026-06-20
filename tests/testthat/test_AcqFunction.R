test_that("AcqFunction API works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)

  acqf = AcqFunction$new(
    id = "acqf",
    constants = ParamSet$new(),
    surrogate = surrogate,
    requires_predict_type_se = FALSE,
    surrogate_class = "SurrogateLearner",
    direction = "same",
    packages = "mlr3mbo",
    label = "label",
    man = "man"
  )

  expect_r6(acqf$codomain, "ParamSet")
  expect_equal(acqf$codomain$ids(), "acqf")
  expect_equal(acqf$surrogate_max_to_min, c(y = 1))
  expect_equal(acqf$direction, "same")
  expect_equal(acqf$domain, inst$search_space)
  expect_learner(acqf$surrogate$learner)
  expect_false(acqf$requires_predict_type_se)
  expect_equal(acqf$packages, "mlr3mbo")
  expect_equal(acqf$label, "label")
  expect_equal(acqf$man, "man")
})

test_that("AcqFunction requires_predict_type_se works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  surrogate$learner$predict_type = "response"

  expect_error(
    AcqFunction$new(
      id = "acqf",
      constants = ParamSet$new(),
      surrogate = surrogate,
      requires_predict_type_se = TRUE,
      surrogate_class = "SurrogateLearner",
      direction = "same"
    ),
    class = "Mlr3ErrorConfig",
    regexp = "requires the surrogate to have"
  )
  surrogate$learner$predict_type = "se"
  acqf = AcqFunction$new(
    id = "acqf",
    constants = ParamSet$new(),
    surrogate = surrogate,
    requires_predict_type_se = TRUE,
    surrogate_class = "SurrogateLearner",
    direction = "same"
  )
  expect_error(
    {
      acqf$surrogate$learner$predict_type = "response"
    },
    class = "Mlr3ErrorConfig",
    regexp = "requires the surrogate to have"
  )
  expect_error(
    {
      acqf$requires_predict_type_se = FALSE
    },
    class = "Mlr3ErrorConfig",
    regexp = "is read-only"
  )
})

test_that("AcqFunction packages works", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)

  expect_warning(
    AcqFunction$new(
      id = "acqf",
      constants = ParamSet$new(),
      surrogate = surrogate,
      packages = "TestPackageThatDoesNotExist",
      requires_predict_type_se = FALSE,
      surrogate_class = "SurrogateLearner",
      direction = "same"
    ),
    "required but not installed for acquisition function"
  )
  acqf = AcqFunction$new(
    id = "acqf",
    constants = ParamSet$new(),
    surrogate = surrogate,
    packages = "mlr3mbo",
    requires_predict_type_se = FALSE,
    surrogate_class = "SurrogateLearner",
    direction = "same"
  )
  expect_equal(acqf$packages, "mlr3mbo")
})

test_that("AcqFunction assert_surrogate rejects wrong surrogate class", {
  inst = MAKE_INST_1D()
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)

  expect_error(
    AcqFunction$new(
      id = "acqf",
      surrogate = surrogate,
      requires_predict_type_se = FALSE,
      surrogate_class = "SurrogateLearnerCollection",
      direction = "same"
    ),
    "SurrogateLearnerCollection"
  )

  acqf = AcqFunction$new(
    id = "acqf",
    surrogate = surrogate,
    requires_predict_type_se = FALSE,
    surrogate_class = "SurrogateLearner",
    direction = "same"
  )
  inst2 = MAKE_INST(OBJ_1D_2, search_space = PS_1D)
  surrogate_collection = srlrn(list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst2$archive)
  expect_error(
    {
      acqf$surrogate = surrogate_collection
    },
    "SurrogateLearner"
  )
})

test_that("AcqFunction assert_surrogate rejects wrong surrogate class in subclasses", {
  inst = MAKE_INST_1D()
  inst2 = MAKE_INST(OBJ_1D_2, search_space = PS_1D)
  surrogate_single = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  surrogate_collection = srlrn(list(REGR_FEATURELESS, REGR_FEATURELESS$clone(deep = TRUE)), archive = inst2$archive)

  expect_error(AcqFunctionEI$new(surrogate = surrogate_collection), "SurrogateLearner")
  expect_error(AcqFunctionCB$new(surrogate = surrogate_collection), "SurrogateLearner")
  expect_error(AcqFunctionEHVI$new(surrogate = surrogate_single), "SurrogateLearnerCollection")
})

test_that("AcqFunction generate_acq_codomain works", {
  inst = MAKE_INST(OBJ_2D, search_space = PS_2D)
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  codomain = generate_acq_codomain(surrogate, "acqf")
  expect_r6(codomain, "ParamSet")
  expect_equal(codomain$tags[["acqf"]], "minimize")
})

test_that("AcqFunction generate_acq_domain works", {
  inst = MAKE_INST(OBJ_2D, search_space = PS_2D)
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  domain = generate_acq_domain(surrogate)
  expect_equal(domain, OBJ_2D$domain)
  expect_equal(domain, inst$search_space)

  inst = MAKE_INST(OBJ_2D, search_space = PS_2D_TRAFO)
  expect_true(inst$search_space$has_trafo)
  surrogate = SurrogateLearner$new(REGR_FEATURELESS, archive = inst$archive)
  domain = generate_acq_domain(surrogate)
  expect_equal(domain, OBJ_2D$domain)
  expect_false(domain$has_trafo)

  surrogate$cols_x = "x2"
  domain = generate_acq_domain(surrogate)
  surrogate$cols_x = "x2"
  domain = generate_acq_domain(surrogate)
  expect_equal(domain, OBJ_2D$domain$clone(deep = TRUE)$subset("x2"))
  expect_false(domain$has_trafo)

  surrogate = SurrogateLearner$new(REGR_FEATURELESS)
  expect_error(generate_acq_domain(surrogate), "Must be an R6 class, not 'NULL'")
})
