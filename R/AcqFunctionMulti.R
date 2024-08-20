#' @title Acquisition Function Wrapping Multiple Acquisition Functions
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_multi
#'
#' @templateVar id multi
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Wrapping multiple [AcqFunction]s resulting in a multi-objective acquisition function composed of the individual ones.
#' Note that the optimization direction of each wrapped acquisition function is corrected for maximization.
#'
#' For each acquisition function, the same [Surrogate] must be used.
#' If acquisition functions passed during construction already have been initialized with a surrogate, it is checked whether
#' the surrogate is the same for all acquisition functions.
#' If acquisition functions have not been initialized with a surrogate, the surrogate passed during construction or lazy initialization
#' will be used for all acquisition functions.
#'
#' For optimization, [AcqOptimizer] can be used as for any other [AcqFunction], however, the [bbotk::Optimizer] wrapped within the [AcqOptimizer]
#' must support multi-objective optimization as indicated via the `multi-crit` property.
#'
#' @family Acquisition Function
#' @export
#' @examples
#' if (requireNamespace("mlr3learners") &
#'     requireNamespace("DiceKriging") &
#'     requireNamespace("rgenoud")) {
#'   library(bbotk)
#'   library(paradox)
#'   library(mlr3learners)
#'   library(data.table)
#'
#'   fun = function(xs) {
#'     list(y = xs$x ^ 2)
#'   }
#'   domain = ps(x = p_dbl(lower = -10, upper = 10))
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#'   objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#'   instance = OptimInstanceBatchSingleCrit$new(
#'     objective = objective,
#'     terminator = trm("evals", n_evals = 5))
#'
#'   instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))
#'
#'   learner = default_gp()
#'
#'   surrogate = srlrn(learner, archive = instance$archive)
#'
#'   acq_function = acqf("multi",
#'     acq_functions = acqfs(c("ei", "pi", "cb")),
#'     surrogate = surrogate
#'   )
#'
#'   acq_function$surrogate$update()
#'   acq_function$update()
#'   acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
#' }
AcqFunctionMulti = R6Class("AcqFunctionMulti",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param acq_functions (list of [AcqFunction]s).
    #' @param surrogate (`NULL` | [Surrogate]).
    initialize = function(acq_functions, surrogate = NULL) {
      assert_list(acq_functions, "AcqFunction", min.len = 2L)
      acq_function_ids = map_chr(acq_functions, function(acq_function) acq_function$id)
      assert_character(acq_function_ids, unique = TRUE)
      acq_functions = setNames(acq_functions, nm = acq_function_ids)
      acq_function_directions = map_chr(acq_functions, function(acq_function) acq_function$direction)
      private$.acq_functions = acq_functions
      private$.acq_function_ids = acq_function_ids
      private$.acq_function_directions = acq_function_directions
      id = paste0(c("acq", map_chr(acq_function_ids, function(id) gsub("acq_", replacement = "", x = id))), collapse = "_")
      label = paste0("Multi Acquisition Function of ", paste0(map_chr(acq_functions, function(acq_function) acq_function$label), collapse = ", "))
      constants = ps()
      domains = map(acq_functions, function(acq_function) acq_function$domain)
      assert_true(all(map_lgl(domains[-1L], function(domain) all.equal(domains[[1L]]$data, domain$data))))
      if (is.null(surrogate)) {
        surrogates = map(acq_functions, function(acq_function) acq_function$surrogate)
        assert_list(surrogates, types = c("Surrogate", "NULL"))
        if (length(unique(map_chr(surrogates, function(surrogate) address(surrogate)))) > 1L) {
          stop("Acquisition functions must rely on the same surrogate model.")
        }
        surrogate = surrogates[[1L]]
      }
      requires_predict_type_se = any(map_lgl(acq_functions, function(acq_function) acq_function$requires_predict_type_se))
      packages = unique(unlist(map(acq_functions, function(acq_function) acq_function$packages)))
      properties = character()
      check_values = FALSE
      man = "mlr3mbo::mlr_acqfunctions_multi"

      private$.requires_predict_type_se = requires_predict_type_se
      private$.packages = packages
      self$direction = "maximize"
      if (is.null(surrogate)) {
        domain = ParamSet$new()
        codomain = ParamSet$new()
      } else {
        if (requires_predict_type_se && surrogate$predict_type != "se") {
          stopf("Acquisition function '%s' requires the surrogate to have `\"se\"` as `$predict_type`.", sprintf("<%s:%s>", "AcqFunction", id))
        }
        private$.surrogate = surrogate
        private$.archive = assert_archive(surrogate$archive)
        for (acq_function in private$.acq_functions) {
          acq_function$surrogate = surrogate
        }
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
      if (length(unique(map_chr(self$acq_functions, function(acq_function) address(acq_function$surrogate)))) > 1L) {
        stop("Acquisition functions must rely on the same surrogate model.")
      }
      for (acq_function in self$acq_functions) {
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
        assert_r6(rhs, classes = "Surrogate")
        if (self$requires_predict_type_se && rhs$predict_type != "se") {
          stopf("Acquisition function '%s' requires the surrogate to have `\"se\"` as `$predict_type`.", format(self))
        }
        private$.surrogate = rhs
        private$.archive = assert_archive(rhs$archive)
        for (acq_function in self$acq_functions) {
          acq_function$surrogate = rhs
        }
        codomain = generate_acq_multi_codomain(rhs, acq_functions = self$acq_functions)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(rhs)
        domain = generate_acq_domain(rhs)
        # lazy initialization requires this:
        self$codomain = Codomain$new(get0("domains", codomain, ifnotfound = codomain$params))  # get0 for old paradox
        self$domain = domain
      }
    },

    #' @field acq_functions (list of [AcqFunction])\cr
    #'   Points to the list of the individual acquisition functions.
    acq_functions = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.acq_functions)) {
        stop("$acq_functions is read-only.")
      }
      private$.acq_functions
    },

    #' @field acq_function_ids (character())\cr
    #'   Points to the ids of the individual acquisition functions.
    acq_function_ids = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.acq_function_ids)) {
        stop("$acq_function_ids is read-only.")
      }
      private$.acq_function_ids
    }
  ),

  private = list(
    .acq_functions = NULL,

    .acq_function_ids = NULL,

    .acq_function_directions = NULL,

    # NOTE: this is currently slower than it could be because when each acquisition functions is evaluated,
    # the mean and se prediction for each point is computed again using the surrogate of that acquisition function,
    # however, as acquisition functions must share the same surrogate, this is redundant.
    # It might be sensible to have a customized eval function for acquisition functions where directly the mean and se
    # predictions are passed (along xdt) so that one can save computing the mean and se predictions over and over again.
    # This also would, however, depend on learners being fully deterministic.
    .fun = function(xdt) {
      values = map_dtc(self$acq_functions, function(acq_function) acq_function$eval_dt(xdt))
      ids = private$.acq_function_ids
      directions = private$.acq_function_directions
      if (any(directions == "same")) {
        directions[directions == "same"] = self$surrogate$archive$codomain$tags[[1L]]
      }
      change_sign = ids[directions == "minimize"]
      for (j in change_sign) {
        set(values, j = j, value = - values[[j]])
      }
      values
    },

    deep_clone = function(name, value) {
      switch(name,
        .acq_functions = value$clone(deep = TRUE),
        value
      )
    }
  )
)

mlr_acqfunctions$add("multi", AcqFunctionMulti)

