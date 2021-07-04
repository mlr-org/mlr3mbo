#' @title Acquisition Function Max-value Entropy Search
#'
#' @description
#' Max-value Entropy Search
#' TODO Currently expects obfun to be maximized!
#'
#' TODO DESCRIPTION and Reference
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

      fun = function(xdt) {
        if (is.null(self$maxes)) {
          stop("maxes is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        mes = map_dbl(seq_len(NROW(p)), function(i) {
          mu = p$mean[i]
          se = p$se[i]
          gamma = (self$maxes - mu) / se
          p_gamma = pnorm(gamma)
          mean(((gamma * dnorm(gamma)) / (2 * p_gamma)) - log(p_gamma), na.rm = TRUE)  # FIXME: check NAs
        })
        mes[is.na(mes)] = 0  # FIXME:
        data.table(acq_mes = mes)
      }

      super$initialize("acq_mes", surrogate = surrogate, direction = "maximize", fun = fun)
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
      self$domain$trafo = NULL # FIXME is it okay to do this?

      self$grid = generate_design_lhs(self$domain, n = 1000L)$data  # FIXME: gridsize
    },

    #' @description
    #' Updates acquisition function and sets `maxes`.
    #'
    #' @param archive [bbotk::Archive]
    update = function(archive) {
      super$update(archive)
      self$maxes = get_maxes(x = archive$data[, archive$cols_x, with = FALSE], grid = self$grid, surrogate = self$surrogate)
    }
  )
)



# FIXME: AcqFunction ParamSet with at least gridsize and nk
get_maxes = function(nK = 1000L, grid, x, surrogate) {
  xgrid = rbind(grid, x)
  p = surrogate$predict(xgrid)
  mu = p$mean
  se = p$se
  se[se < .Machine$double.eps] = .Machine$double.eps  # FIXME:
  mu_max = max(mu)

  left = mu_max
  leftprob = probf(left, mu = mu, se = se)
  while (leftprob > 0.1) {
    left = if (left > 0.01) left / 2 else 2 * left - 0.05
    leftprob = probf(left, mu = mu, se = se)
  }

  right = max(mu + 5 * se)
  rightprob = probf(right, mu = mu, se = se)
  while (rightprob < 0.95) {
    right = right + right - left
    rightprob = probf(right, mu = mu, se = se)
  }

  mgrid = seq(from = left, to = right, length.out = 100L)

  prob = apply(pnorm(
    (matrix(mgrid, nrow = NROW(mu), ncol = 100L, byrow = TRUE) - matrix(mu, nrow = NROW(mu), ncol = 100L)) /
    matrix(se, nrow = NROW(mu), ncol = 100L)),
  MARGIN = 1L, FUN = prod)

  if (sum(prob > 0.05 & prob < 0.95) == 0L) {
    return(mu_max + runif(nK, min = 0, max = 1))
  }

  # Gumbel sampling
  q1 = optimize(function(x) abs(probf(x, mu = mu, se = se) - 0.25), interval = range(mgrid))$minimum
  q2 = optimize(function(x) abs(probf(x, mu = mu, se = se) - 0.5), interval = range(mgrid))$minimum
  q3 = optimize(function(x) abs(probf(x, mu = mu, se = se) - 0.75), interval = range(mgrid))$minimum
  beta = (q1 - q3) / (log(log(4 / 3)) - log(log(4)))  # FIXME: assert beta > 0
  alpha = q2 + beta * log(log(2))

  -log(-log(runif(nK, min = 0, max = 1))) * beta + alpha
  # FIXME: maxes that are <= mu_max + eps should be replaced by mu_max + eps
}



probf = function(mu_, mu, se) {
  prod(pnorm((mu_ - mu) / se))
}

