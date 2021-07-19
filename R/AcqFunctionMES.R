#' @title Acquisition Function Max-value Entropy Search
#'
#' @description
#' Max-value Entropy Search
#'
#' # FIXME: DESCRIPTION and Reference, tests
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionMES = R6Class("AcqFunctionMES",
  inherit = AcqFunction,
  public = list(

    #' @field grid [data.table::data.table]
    grid = NULL,

    #' @field maxes (`numeric()`).
    maxes = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateSingleCrit")

      ps = ps(resolution = p_int(lower = 1L, default = 10000L), n_maxes = p_int(lower = 1L, default = 100L))  # FIXME: resolution should depend on dimensionality of problem
      ps$values = list(resolution = 1000L, n_maxes = 100L)

      fun = function(xdt) {
        if (is.null(self$maxes)) {
          stop("maxes is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        # FIXME: do this in matrix operations
        mes = map_dbl(seq_len(nrow(p)), function(i) {
          mu = p$mean[i]
          se = p$se[i]
          gamma = (self$maxes - (- self$surrogate_max_to_min * mu)) / se
          p_gamma = pnorm(gamma)
          mean(((gamma * dnorm(gamma)) / (2 * p_gamma)) - log(p_gamma), na.rm = TRUE)
        })
        mes[is.na(mes)] = 0  # FIXME: check why NAs can occur
        data.table(acq_mes = mes)
      }

      super$initialize("acq_mes", surrogate = surrogate, direction = "maximize", fun = fun, param_set = ps)
    },

    #' @description
    #' Sets up the acquisition function.
    #'
    #' @param archive [bbotk::Archive].
    setup = function(archive) {
      # FIXME: Should we allow alternative search_space as additional argument?

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive$codomain, id = self$id, direction = self$direction)

      self$surrogate_max_to_min = mult_max_to_min(archive$codomain)

      self$domain = archive$search_space$clone(deep = TRUE)
      self$domain$trafo = NULL # FIXME: is it okay to do this?

      if (is.null(self$grid)) {
        self$grid = generate_design_grid(self$domain, resolution = self$param_set$values$resolution)$data
      }
    },

    #' @description
    #' Updates acquisition function and sets `maxes`.
    #'
    #' @param archive [bbotk::Archive]
    update = function(archive) {
      super$update(archive)
      x = archive$data[, archive$cols_x, with = FALSE]  # FIXME: additional safety checks here?
      self$maxes = sample_maxes_gumbel(x = x, grid = self$grid, surrogate = self$surrogate, surrogate_max_to_min = self$surrogate_max_to_min, n_maxes = self$param_set$values$n_maxes)
    }
  )
)



# FIXME: document
# this is written in the perspective of maximization, i.e., surrogate_max_to_min = -1
sample_maxes_gumbel = function(x, grid, surrogate, surrogate_max_to_min, n_maxes) {
  xgrid = rbind(grid, x)
  n_grid = nrow(xgrid)
  p = surrogate$predict(xgrid)
  mu = p$mean
  se = p$se
  se[se < .Machine$double.eps] = .Machine$double.eps
  mu_max = max(- surrogate_max_to_min * mu)

  left = mu_max
  if (probf(left, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) < 0.25) {
    right = max(- surrogate_max_to_min * (mu - (surrogate_max_to_min * 5 * se)))
    while (probf(right, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) < 0.75) {
      right = 2 * right - left
    }

    mgrid = seq(from = left, to = right, length.out = n_maxes)

    Z = matrix(mgrid, nrow = n_grid, ncol = n_maxes, byrow = TRUE) - (- surrogate_max_to_min * matrix(mu, nrow = n_grid, ncol = n_maxes)) / matrix(se, nrow = n_grid, ncol = n_maxes)

    prob = apply(pnorm(Z), MARGIN = 1L, FUN = prod)


    if (sum(prob > 0.05 & prob < 0.95) == 0L) {
      return(mu_max + runif(n_maxes, min = 0, max = 1))  # FIXME: some heuristic
    }

    # inverse Gumbel sampling
    q1 = optimize(function(x) abs(probf(x, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) - 0.25), interval = range(mgrid))$minimum
    q2 = optimize(function(x) abs(probf(x, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) - 0.50), interval = range(mgrid))$minimum
    q3 = optimize(function(x) abs(probf(x, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) - 0.75), interval = range(mgrid))$minimum
    beta = (q1 - q3) / (log(log(4 / 3)) - log(log(4)))
    if (beta < .Machine$double.eps) beta = sqrt(.Machine$double.eps)  # FIXME: some heuristic
    alpha = q2 + beta * log(log(2))

    - log(- log(runif(n_maxes, min = 0, max = 1))) * beta + alpha  # FIXME: https://github.com/zi-w/Max-value-Entropy-Search/blob/master/acFuns/mesg_choose.m l61
  } else {
    left + 5 * sd(mu) # FIXME: https://github.com/zi-w/Max-value-Entropy-Search/blob/master/acFuns/mesg_choose.m l62
  }
}



# FIXME: document
# this is written in the perspective of maximization, i.e., surrogate_max_to_min = -1
probf = function(x, mu, se, surrogate_max_to_min) {
  prod(pnorm((x - (- surrogate_max_to_min * mu)) / se))
}


if (FALSE) {
  set.seed(1)
  library(bbotk)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  obfun = ObjectiveRFun$new(
    fun = function(xs) list(y = sum(unlist(xs)^2)),
    domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
    codomain = ParamSet$new(list(ParamDbl$new("y", tags = "minimize"))),
    id = "test"
  )

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km", optim.method = "gen"))
  acq_function = AcqFunctionMES$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("grid_search", resolution = 1000, batch_size = 1000), trm("evals", n_evals = 1000))

  bayesopt_soo(instance, acq_function, acq_optimizer)
}
