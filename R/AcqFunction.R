#' @title Acquisition Function Base Class
#'
#' @include mlr_acqfunctions.R
#'
#' @description
#' Abstract acquisition function class.
#'
#' Based on the predictions of a [Surrogate], the acquisition function encodes the preference to evaluate a new point.
#'
#' @family Acquisition Function
#' @export
AcqFunction = R6Class("AcqFunction",
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
    #' @param direction (`"same"` | `"minimize"` | `"maximize"`).
    #'   Optimization direction of the acquisition function relative to the direction of the
    #'   objective function of the [bbotk::OptimInstance].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled prior to construction if at least one of the packages is not installed, but loaded (not attached) later on-demand via [requireNamespace()].
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(id, constants = ParamSet$new(), surrogate, requires_predict_type_se, direction, packages = NULL, label = NA_character_, man = NA_character_) {
      # FIXME: Should we allow alternative search_space as additional argument?
      # If we do, we need to trafo values before updating the surrogate and predicting?
      assert_string(id)
      assert_r6(surrogate, classes = "Surrogate", null.ok = TRUE)
      assert_character(packages, null.ok = TRUE)
      if (!is.null(packages)) {
        check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for acquisition function '%s'", sprintf("<%s:%s>", "AcqFunction", id)))
      }
      private$.requires_predict_type_se = assert_flag(requires_predict_type_se)
      private$.packages = packages
      self$direction = assert_choice(direction, c("same", "minimize", "maximize"))
      if (is.null(surrogate)) {
        domain = ParamSet$new()
        codomain = ParamSet$new()
      } else {
        if (requires_predict_type_se && surrogate$predict_type != "se") {
          stopf("Acquisition function '%s' requires the surrogate to have `\"se\"` as `$predict_type`.", sprintf("<%s:%s>", "AcqFunction", id))
        }
        private$.surrogate = surrogate
        private$.archive = assert_r6(surrogate$archive, classes = "Archive")
        codomain = generate_acq_codomain(surrogate, id = id, direction = direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(surrogate)
        domain = generate_acq_domain(surrogate)
      }
      super$initialize(id = id, domain = domain, codomain = codomain, constants = constants, label = label, man = man)
    },

    #' @description
    #' Update the acquisition function.
    #'
    #' Can be implemented by subclasses.
    update = function() {
      # FIXME: at some point we may want to make this an AB to a private$.update
    },

    #' @description
    #' Evaluates multiple input values on the objective function.
    #'
    #' @param xss (`list()`)\cr
    #'   A list of lists that contains multiple x values, e.g.
    #'   `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.
    #'
    #' @return data.table::data.table() that contains one y-column for
    #' single-objective functions and multiple y-columns for multi-objective functions,
    #' e.g. `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_many = function(xss) {
      if (self$check_values) lapply(xss, self$domain$assert)
      res = invoke(private$.fun, rbindlist(xss, use.names = TRUE, fill = TRUE), .args = self$constants$values)
      if (self$check_values) self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      res
    },

    #' @description
    #' Evaluates multiple input values on the objective function
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   One point per row, e.g. `data.table(x1 = c(1, 3), x2 = c(2, 4))`.
    #'
    #' @return data.table::data.table() that contains one y-column for
    #' single-objective functions and multiple y-columns for multi-objective
    #' functions, e.g. `data.table(y = 1:2)` or `data.table(y1 = 1:2, y2 = 3:4)`.
    eval_dt = function(xdt) {
      if (self$check_values) self$domain$assert_dt(xdt)
      res = invoke(private$.fun, xdt, .args = self$constants$values)
      if (self$check_values) self$codomain$assert_dt(res[, self$codomain$ids(), with = FALSE])
      res
    }
  ),

  active = list(
    #' @field direction (`"same"` | `"minimize"` | `"maximize"`)\cr
    #'   Optimization direction of the acquisition function relative to the direction of the
    #'   objective function of the [bbotk::OptimInstance].
    #'   Must be `"same"`, `"minimize"`, or `"maximize"`.
    direction = function(rhs) {
      if (missing(rhs)) {
        private$.direction
      } else {
        private$.direction = assert_choice(rhs, choices = c("same", "minimize", "maximize"))
      }
    },

    #' @field surrogate_max_to_min (`-1` | `1`)\cr
    #'   Multiplicative factor to correct for minimization or maximization of the acquisition
    #'   function.
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
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("$archive is read-only.")
      private$.archive
    },

    #' @field fun (`function`)\cr
    #'   Points to the private acquisition function to be implemented by subclasses.
    fun = function(lhs) {
      if (!missing(lhs) && !identical(lhs, private$.fun)) stop("$fun is read-only.")
      private$.fun
    },

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
        codomain = generate_acq_codomain(rhs, id = self$id, direction = self$direction)
        self$surrogate_max_to_min = surrogate_mult_max_to_min(rhs)
        domain = generate_acq_domain(rhs)
        # lazy initialization requires this:
        self$codomain = Codomain$new(get0("domains", codomain, ifnotfound = codomain$params))  # get0 for old paradox
        self$domain = domain
      }
    },

    #' @field requires_predict_type_se (`logical(1)`)\cr
    #'   Whether the acquisition function requires the surrogate to have `"se"` as `$predict_type`.
    requires_predict_type_se = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.requires_predict_type_se)) {
        stop("$requires_predict_type_se is read-only.")
      }
      private$.requires_predict_type_se
    },

    #' @field packages (`character()`)\cr
    #'   Set of required packages.
    packages = function(rhs) {
      if (missing(rhs)) {
        private$.packages
      } else {
        stop("$packages is read-only.")
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

    .packages = NULL
  )
)

