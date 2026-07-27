#' @title Acquisition Function Base Class
#'
#' @include mlr_acqfunctions.R
#'
#' @description
#' Abstract acquisition function class.
#'
#' Based on the predictions of a [Surrogate], the acquisition function encodes the preference to evaluate a new point.
#'
#' Most acquisition functions are stateful and depend on quantities that must be recomputed whenever the [Surrogate]
#' has been refitted on new data, e.g., the best objective function value observed so far (`$y_best` of
#' [mlr_acqfunctions_ei]) or the current Pareto front and the reference point (`$ys_front` and `$ref_point` of
#' [mlr_acqfunctions_ehvi]).
#' These quantities are cached in public fields and are recomputed from the [Surrogate] and its [bbotk::Archive] by
#' calling `$update()`.
#' Which fields a subclass sets is documented in its `$update()` method.
#'
#' Loop functions such as [bayesopt_ego] call `$update()` in every iteration, directly after updating the surrogate
#' and before optimizing the acquisition function:
#'
#' ```
#' acq_function$surrogate$update()
#' acq_function$update()
#' acq_optimizer$optimize()
#' ```
#'
#' The order matters, because `$update()` reads the archive through the surrogate and may rely on the surrogate's
#' predictions or its [OutputTrafo].
#' Evaluating an acquisition function whose cached fields have not been set results in an error along the lines of
#' `"$y_best is not set. Missed to call $update()?"`.
#'
#' `$reset()` discards state so that the same acquisition function object can be reused for another optimization run
#' without carrying over information from the previous one.
#' Fields that `$update()` recomputes from scratch in every iteration need not be reset,
#' which is why most acquisition functions do not override `$reset()`.
#' It matters for state that persists across iterations instead:
#' [mlr_acqfunctions_stochastic_cb], for example, samples `lambda` once at the first `$update()` and afterwards only
#' decays it using an iteration counter,
#' so both are reset to make the next run start from a freshly sampled `lambda`.
#' [OptimizerMbo] and [OptimizerAsyncMbo] call `$reset()` at the beginning of `$optimize()`, together with resetting
#' the [Surrogate] and the [AcqOptimizer].
#'
#' Both methods can be implemented by subclasses.
#' The default implementations do nothing, which is sufficient for stateless acquisition functions such as
#' [mlr_acqfunctions_mean] or [mlr_acqfunctions_sd].
#'
#' @family Acquisition Function
#' @export
AcqFunction = R6Class(
  "AcqFunction",
  inherit = bbotk::Objective,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that the surrogate can be initialized lazy and can later be set via the active binding `$surrogate`.
    #'
    #' @param id (`character(1)`).
    #' @param constants ([paradox::ParamSet]).
    #'   Changeable constants or parameters.
    #' @param surrogate (`NULL` | [Surrogate]).
    #'   Surrogate whose predictions are used in the acquisition function.
    #' @param requires_predict_type_se (`logical(1)`)\cr
    #'   Whether the acquisition function requires the surrogate to have `"se"` as `$predict_type`.
    #' @param surrogate_class (`character(1)`)\cr
    #'   Allowed class of the surrogate.
    #' @param direction (`"same"` | `"minimize"` | `"maximize"`).
    #'   Optimization direction of the acquisition function relative to the direction of
    #'   the objective function of the [bbotk::OptimInstance].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled prior to construction if at least one of the packages is not installed,
    #'   but loaded (not attached) later on-demand via [requireNamespace()].
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(
      id,
      constants = ParamSet$new(),
      surrogate = NULL,
      requires_predict_type_se,
      surrogate_class,
      direction,
      packages = NULL,
      label = NA_character_,
      man = NA_character_
    ) {
      # FIXME: Should we allow alternative search_space as additional argument?
      # If we do, we need to trafo values before updating the surrogate and predicting?
      assert_string(id)
      assert_character(packages, null.ok = TRUE)
      if (!is.null(packages)) {
        check_packages_installed(
          packages,
          msg = sprintf(
            "Package '%%s' required but not installed for acquisition function '%s'",
            sprintf("<%s:%s>", "AcqFunction", id)
          )
        )
      }
      private$.requires_predict_type_se = assert_flag(requires_predict_type_se)
      private$.surrogate_class = assert_string(surrogate_class)
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
      if (is.null(surrogate)) {
        domain = ParamSet$new()
        codomain = ParamSet$new()
      } else {
        self$assert_surrogate(surrogate)
        private$.surrogate = surrogate
        private$.archive = assert_archive(surrogate$archive)
        codomain = generate_acq_codomain(surrogate, id = id, direction = direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(surrogate)
        domain = generate_acq_domain(surrogate)
      }
      super$initialize(
        id = id,
        domain = domain,
        codomain = codomain,
        constants = constants,
        check_values = FALSE,
        label = label,
        man = man
      )
      # workaround for bbotk
      # move to initialize after bbotk CRAN update
      private$.packages = packages
    },

    #' @description
    #' Update the acquisition function.
    #' Recomputes the cached quantities from the current state of the [Surrogate] and its [bbotk::Archive].
    #' Can be implemented by subclasses; see the class description above for details.
    #'
    #' @return `NULL`.
    update = function() {
      # FIXME: at some point we may want to make this an AB to a private$.update
      invisible(NULL)
    },

    #' @description
    #' Reset the acquisition function.
    #' Discards state so that the acquisition function can be reused for another optimization run.
    #' Can be implemented by subclasses; see the class description above for details.
    #'
    #' @return `NULL`.
    reset = function() {
      # FIXME: at some point we may want to make this an AB to a private$.reset
      invisible(NULL)
    },

    #' @description
    #' Evaluates multiple input values on the acquisition function.
    #'
    #' @param xss (`list()`)\cr
    #'   A list of lists that contains multiple x values, e.g.
    #'   `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.
    #'
    #' @return data.table::data.table() that contains one y-column for
    #' single-objective acquisition functions and multiple y-columns for multi-objective
    #' acquisition functions, e.g. `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_many = function(xss) {
      if (self$check_values) {
        lapply(xss, self$domain$assert)
      }
      res = invoke(private$.fun, rbindlist(xss, use.names = TRUE, fill = TRUE), .args = self$constants$values)
      if (self$check_values) {
        self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      }
      res
    },

    #' @description
    #' Evaluates multiple input values on the objective function
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   One point per row, e.g. `data.table(x1 = c(1, 3), x2 = c(2, 4))`.
    #'
    #' @return data.table::data.table() that contains one y-column for
    #' single-objective acquisition functions and multiple y-columns for multi-objective
    #' acquisition functions, e.g. `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_dt = function(xdt) {
      if (self$check_values) {
        self$domain$assert_dt(xdt)
      }
      res = invoke(private$.fun, xdt, .args = self$constants$values)
      if (self$check_values) {
        self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      }
      res
    },

    #' @description
    #' Validate that the surrogate is compatible with this acquisition function.
    #' Asserts the surrogate class and that `$predict_type` is `"se"` if required.
    #' Subclasses with additional requirements must override this method.
    #'
    #' @param surrogate ([Surrogate])\cr
    #'   Surrogate to validate.
    #'
    #' @return The validated [Surrogate].
    assert_surrogate = function(surrogate) {
      assert_r6(surrogate, classes = private$.surrogate_class)
      if (self$requires_predict_type_se && surrogate$predict_type != "se") {
        error_config(
          "Acquisition function '%s' requires the surrogate to have 'se' as predict_type.",
          class(self)[[1L]]
        )
      }
      surrogate
    }
  ),

  active = list(
    #' @field direction (`"same"` | `"minimize"` | `"maximize"`)\cr
    #'   Optimization direction of the acquisition function relative to the direction of
    #'   the objective function of the [bbotk::OptimInstance] related to the passed [bbotk::Archive].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    direction = function(rhs) {
      if (missing(rhs)) {
        private$.direction
      } else {
        private$.direction = assert_choice(rhs, choices = c("same", "minimize", "maximize"))
      }
    },

    #' @field surrogate_max_to_min (`-1` | `1`)\cr
    #'   Multiplicative factor to correct for minimization or maximization of
    #'   the acquisition function.
    surrogate_max_to_min = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate_max_to_min
      } else {
        private$.surrogate_max_to_min = assert_subset(rhs, choices = c(-1L, 1L))
      }
    },

    #' @field label (`character(1)`)\cr
    #'   Label for this object.
    label = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.label)) {
        stop("$label is read-only.")
      }
      private$.label
    },

    #' @field man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    man = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.man)) {
        stop("$man is read-only.")
      }
      private$.man
    },

    #' @field archive ([bbotk::Archive])\cr
    #'   Points to the [bbotk::Archive] of the surrogate.
    archive = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) {
        stop("$archive is read-only.")
      }
      private$.archive
    },

    #' @field fun (`function`)\cr
    #'   Points to the private acquisition function to be implemented by subclasses.
    fun = function(lhs) {
      if (!missing(lhs) && !identical(lhs, private$.fun)) {
        stop("$fun is read-only.")
      }
      private$.fun
    },

    #' @field surrogate ([Surrogate])\cr
    #'  Surrogate.
    surrogate = function(rhs) {
      if (missing(rhs)) {
        private$.surrogate
      } else {
        self$assert_surrogate(rhs)
        private$.surrogate = rhs
        private$.archive = assert_archive(rhs$archive)
        codomain = generate_acq_codomain(rhs, id = self$id, direction = self$direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(rhs)
        domain = generate_acq_domain(rhs)
        # lazy initialization requires this:
        self$codomain = Codomain$new(codomain$domains)
        self$domain = domain
      }
    },

    #' @field requires_predict_type_se (`logical(1)`)\cr
    #'   Whether the acquisition function requires the surrogate to have `"se"` as `$predict_type`.
    requires_predict_type_se = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.requires_predict_type_se)) {
        error_config("$requires_predict_type_se is read-only.")
      }
      private$.requires_predict_type_se
    },

    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    packages = function(rhs) {
      if (missing(rhs)) {
        private$.packages
      } else {
        error_config("$packages is read-only.")
      }
    }
  ),

  private = list(
    .direction = NULL,

    .surrogate_max_to_min = NULL,

    .label = NULL,

    .man = NULL,

    .archive = NULL,

    .fun = function(xdt) {
      stop("Abstract.")
    },

    .surrogate = NULL,

    .requires_predict_type_se = NULL,

    .surrogate_class = NULL,

    .packages = NULL
  )
)
