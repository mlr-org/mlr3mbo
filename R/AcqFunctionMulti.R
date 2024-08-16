AcqFunctionMulti = R6Class("AcqFunctionMulti",
  inherit = AcqFunction,

  public = list(

    initialize = function(acq_functions) {
      assert_list(acq_functions, "AcqFunction", min.len = 2L)
      private$.acq_functions = acq_functions
      # FIXME: check for unique ids
      id = paste0(c("acq", map_chr(acq_functions, function(acq_function) gsub("acq_", replacement = "", x = acq_function$id))), collapse = "_")
      label = paste0("Multi Acquisition Function of ", paste0(map_chr(acq_functions, function(acq_function) acq_function$label), collapse = ", "))
      # FIXME: constant ids must be prefixed by acqf_id
      constants = do.call(c, map(acq_functions, function(acq_function) acq_function$constants))
      domains = map(acq_functions, function(acq_function) acq_function$domain)
      assert_true(all(map_lgl(domains[-1L], function(domain) all.equal(domains[[1L]]$data, domain$data))))
      # FIXME: surrogates could be the same or different, how to handle this with $update() of the surrogate?
      surrogates = map(acq_functions, function(acq_function) acq_function$surrogate)
      assert_true(length(unique(map_chr(surrogates, function(surrogate) address(surrogate)))) == 1L)
      surrogate = surrogates[[1L]]
      requires_predict_type_se = any(map_lgl(acq_functions, function(acq_function) acq_function$requires_predict_type_se))
      packages = unique(unlist(map(acq_functions, function(acq_function) acq_function$packages)))
      properties = character()
      check_values = FALSE
      man = "mlr3mbo::mlr_acqfunctions_multi"

      private$.requires_predict_type_se = requires_predict_type_se
      private$.packages = packages
      self$direction = "minimize"
      if (is.null(surrogate)) {
        domain = ParamSet$new()
        codomain = ParamSet$new()
      } else {
        if (requires_predict_type_se && surrogate$predict_type != "se") {
          stopf("Acquisition function '%s' requires the surrogate to have `\"se\"` as `$predict_type`.", sprintf("<%s:%s>", "AcqFunction", id))
        }
        private$.surrogate = surrogate
        private$.archive = assert_r6(surrogate$archive, classes = "Archive")
        codomain = generate_acq_multi_codomain(surrogate, acq_functions = acq_functions)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(surrogate)
        domain = generate_acq_domain(surrogate)
      }

      self$id = assert_string(id)
      self$domain = assert_param_set(domain)
      assert_param_set(codomain)
      # get "codomain" element if present (new paradox) or default to $params (old paradox)
      params = get0("domains", codomain, ifnotfound = codomain$params)
      self$codomain = Codomain$new(params)
      assert_names(self$domain$ids(), disjunct.from = self$codomain$ids())
      assert_names(self$domain$ids(), disjunct.from = c("x_domain", "timestamp", "batch_nr"))
      assert_names(self$codomain$ids(), disjunct.from = c("x_domain", "timestamp", "batch_nr"))
      self$properties = assert_subset(properties, bbotk_reflections$objective_properties)
      self$constants = assert_param_set(constants)
      self$check_values = assert_flag(check_values)
      private$.label = assert_string(label, na.ok = TRUE)
      private$.man = assert_string(man, na.ok = TRUE)
    },

    #' @description
    #' Update each of the wrapped acquisition functions.
    update = function() {
      for (acq_function in private$.acq_functions) {
        acq_function$update()
      }
    }
  ),

  active = list(
    #' @field surrogate ([Surrogate])\cr
    #'  Surrogate.
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        # FIXME: assign surrogate to all acqfs?
        assert_r6(rhs, classes = "Surrogate")
        if (self$requires_predict_type_se && rhs$predict_type != "se") {
          stopf("Acquisition function '%s' requires the surrogate to have `\"se\"` as `$predict_type`.", format(self))
        }
        private$.surrogate = rhs
        private$.archive = assert_archive(rhs$archive)
        codomain = generate_acq_multi_codomain(surrogate, acq_functions = acq_functions)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(rhs)
        domain = generate_acq_domain(rhs)
        # lazy initialization requires this:
        self$codomain = Codomain$new(get0("domains", codomain, ifnotfound = codomain$params))  # get0 for old paradox
        self$domain = domain
      }
    }
  ),

  private = list(
    .acq_functions = NULL,

    .fun = function(xdt, ...) {
      constants = list(...)
      # FIXME: prefixed constants matching; needed at all?
      values = map_dtc(private$.acq_functions, function(acq_function) acq_function$eval_dt(xdt))
      ids = map_chr(private$.acq_functions, function(acq_function) acq_function$id)
      directions = map_chr(private$.acq_functions, function(acq_function) acq_function$direction)
      if (any(directions == "same")) {
        directions[directions == "same"] = self$surrogate$archive$codomain$tags[[1L]]
      }
      change_sign = ids[directions == "maximize"]
      for (j in change_sign) {
        set(values, j = j, value = - values[[j]])
      }
      # FIXME: standardize column ranges to [0, 1]
      values
    }
  )
)

# FIXME: test with multi objective also?
# FIXME: currently there is overhead because each acqf predicts with the surrogate
# but if the surrogate is always the same and shared, can we save time by predicting for xdt
# and having for each acqf a variant of fun that directly uses mean and se

if (FALSE) {
  fun = function(xs) {
    list(y = xs$x ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))

  learner = default_gp()

  surrogate = srlrn(learner, archive = instance$archive)

  ei = acqf("ei", surrogate = surrogate)
  lcb = acqf("cb", surrogate = surrogate)
  pi = acqf("pi", surrogate = surrogate)

  acqf = AcqFunctionMulti$new(list(ei, lcb, pi))
  acqf$surrogate$update()
  acqf$update()
  acqf$eval_dt(data.table(x = c(-1, 0, 1)))
}

