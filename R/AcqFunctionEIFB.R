#' @title Acquisition Function Expected Improvement Fully Bayesian
#'
#' @description
#' Expected Improvement Fully Bayesian.
#'
#' TODO DESCRIPTION and Reference
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionEIFB = R6Class("AcqFunctionEIFB",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @field archive [data.table::data.table].
    archive = NULL,


    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateSingleCrit")

      fun = function(xdt) {
        if (is.null(self$y_best)) {
          stop("y_best is not set. Missed to call $update(archive)?")
        }
        ei = map(seq_len(100), .f = function(i) {  # FIXME: number of iters
          p = slice_sampling(self$surrogate$clone(deep = TRUE), xdt = xdt, archive = self$archive)
          mu = p$mean
          se = p$se
          d = self$y_best - self$surrogate_max_to_min * mu
          d_norm = d / se
          ei = d * pnorm(d_norm) + se * dnorm(d_norm)
          ei = ifelse(se < 1e-20, 0, ei)
        })
        data.table(acq_eifb = rowMeans(setDT(ei)))
      }

      super$initialize("acq_eifb", surrogate = surrogate, direction = "maximize", fun = fun)
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    #'
    #' @param archive [bbotk::Archive]
    update = function(archive) {
      super$update(archive)
      self$y_best = archive$best()[[archive$cols_y]]
      self$archive= archive
    }
  )
)



slice_sampling = function(surrogate, xdt, archive) {
  assert_r6(surrogate$model, classes = "LearnerRegrKM")
  model = surrogate$model$model
  coefs = Filter(function(x) length(x) > 0L, DiceKriging::coef(model@covariance))
  new_coefs = map(names(coefs), .f = function(theta_id) {
    slice_sampling_inner(theta_id, surrogate = surrogate)
  })
  names(new_coefs) = map_chr(new_coefs, names)
  surrogate$model$param_set$values$coef.cov = unlist(new_coefs[names(new_coefs) != "sd2"])
  surrogate$model$param_set$values$coef.var = new_coefs[["sd2"]]
  surrogate$update(xydt = archive_xy(archive), y_cols = archive$cols_y)
  surrogate$predict(xdt)
}



slice_sampling_inner = function(theta_id, surrogate, sigma = 1L) {
  # Murray & Adams 2010
  # FIXME: only LLconcentration_beta_sigma2
  # FIXME: check all kernel pars
  model = surrogate$model$model
  theta_orig = DiceKriging::coef(model@covariance)
  theta = setNames(theta_orig[[theta_id]], nm = theta_id)
  f = model@y

  S_theta = diag(1, nrow = model@n, ncol = model@n)  # FIXME:
  Sigma_theta = DiceKriging::covMatrix(model@covariance, X = model@X, noise.var = model@noise.var)[["C"]]
  R_theta = S_theta - S_theta %*% solve((S_theta + Sigma_theta)) %*% S_theta
  L_R_theta = t(chol(R_theta))

  g = mvtnorm::rmvnorm(1L, mean = f, sigma = S_theta)
  m_theta_g = R_theta %*% solve(S_theta) %*% t(g)

  eta = solve(L_R_theta) %*% (f - m_theta_g)

  v = runif(1L, min = 0, max = sigma)
  theta_min = theta - v
  if (theta_min < 0) theta_min = 0
  theta_max = theta_min + sigma

  ################################################################################################# subroutine
  go = TRUE
  while (go) {
    u = runif(1L, min = 0, max = 1)

    y = u * model@logLik * mvtnorm::dmvnorm(g, mean = rep(0, model@n), sigma = Sigma_theta + S_theta) * punif(theta, min = theta_min, max = theta_max)

    theta_ = setNames(runif(1, min = theta_min, max = theta_max), nm = names(theta))
    # FIXME:
    coef.cov = if (theta_id == "sd2") unlist(theta_orig) else unlist(insert_named(theta_orig, theta_))
    coef.cov = coef.cov[names(coef.cov) != "sd2"]
    # FIXME:
    covstruct = DiceKriging::covStruct.create(
      covtype = surrogate$model$param_set$values$covtype %??% "matern5_2",
      d = model@covariance@d,
      known.covparam = model@covariance@known.covparam,
      var.names = model@covariance@var.names,
      coef.cov = coef.cov,
      coef.var = surrogate$model$param_set$values$coef.var,
      nugget = surrogate$model$param_set$values$nugget,
      nugget.estim = model@covariance@nugget.estim,
      nugget.flag = model@covariance@nugget.flag,
      iso = model@control$iso %??% FALSE,
      scaling = model@control$scaling %??% FALSE,
      knots = NULL,
      kernel = NULL)

    if (theta_id == "sd2") covstruct@sd2 = theta_ else if ("sd2" %in% names(theta_orig)) covstruct@sd2 = theta_orig[["sd2"]]  # FIXME:
    Sigma_theta_ = DiceKriging::covMatrix(covstruct, X = model@X, noise.var = model@noise.var)[["C"]]
    S_theta_ = S_theta  # FIXME:
    R_theta_ = S_theta_ - S_theta_ %*% solve((S_theta_ + Sigma_theta_)) %*% S_theta_
    L_R_theta_ = t(chol(R_theta_))
    m_theta_g_ = R_theta_ %*% solve(S_theta_) %*% t(g)
    f_ = L_R_theta_ %*% eta + m_theta_g_

    logLik_ = DiceKriging:::logLikFun(theta_, model = model, envir = NULL)

    if (logLik_ * mvtnorm::dmvnorm(g, mean = rep(0, model@n), sigma = Sigma_theta_ + S_theta_) * punif(theta_, min = theta_min, max = theta_max) > y) {
      go = FALSE
    } else if (theta_ < theta) {
      theta_min = theta_
    } else {
      theta_max = theta_
    }
  }
  theta_
}

