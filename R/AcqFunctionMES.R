#' @title Acquisition Function Max-value Entropy Search
#'
#' @description
#' Max-value Entropy Search
#'
#' FIXME: DESCRIPTION and Reference, tests
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
      # FIXME: maybe restrict this to GPs and purely numerical search spaces and deterministic objective
      assert_r6(surrogate, "SurrogateSingleCrit")

      ps = ps(grid_size = p_int(lower = 1L, default = 10000L), n_maxes = p_int(lower = 1L, default = 100L))
      ps$values = list(grid_size = 10000L, n_maxes = 100L)

      fun = function(xdt) {
        if (is.null(self$maxes)) {
          stop("maxes is not set. Missed to call $update(archive)?")
        }
        p = self$surrogate$predict(xdt)
        # Note: we probably do not want to do this in matrix operations because of bad scaling in nrow(p)
        mes = map_dbl(seq_len(nrow(p)), function(i) {
          mu = p$mean[i]
          se = p$se[i]
          gamma = (self$maxes - (- self$surrogate_max_to_min * mu)) / se
          p_gamma = stats::pnorm(gamma)
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
        resolution = ceiling(self$param_set$values$grid_size ^ (1 / sum(self$domain$is_number)))
        self$grid = generate_design_grid(self$domain, resolution = resolution)$data
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

    prob = apply(stats::pnorm(Z), MARGIN = 1L, FUN = prod)

    # inverse Gumbel sampling
    q1 = stats::optimize(function(x) abs(probf(x, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) - 0.25), interval = range(mgrid))$minimum
    q2 = stats::optimize(function(x) abs(probf(x, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) - 0.50), interval = range(mgrid))$minimum
    q3 = stats::optimize(function(x) abs(probf(x, mu = mu, se = se, surrogate_max_to_min = surrogate_max_to_min) - 0.75), interval = range(mgrid))$minimum
    beta = (q1 - q3) / (log(log(4 / 3)) - log(log(4)))
    if (beta < .Machine$double.eps) beta = sqrt(.Machine$double.eps)  # FIXME: some heuristic
    alpha = q2 + beta * log(log(2))

    - log(- log(runif(n_maxes, min = 0, max = 1))) * beta + alpha  # FIXME: https://github.com/zi-w/Max-value-Entropy-Search/blob/master/acFuns/mesg_choose.m l61
  } else {
    left + stats::sd(mu) # FIXME: https://github.com/zi-w/Max-value-Entropy-Search/blob/master/acFuns/mesg_choose.m l62
  }
}



# FIXME: document
# this is written in the perspective of maximization, i.e., surrogate_max_to_min = -1
probf = function(x, mu, se, surrogate_max_to_min) {
  prod(stats::pnorm((x - (- surrogate_max_to_min * mu)) / se))
}


if (FALSE) {
  set.seed(1)
  library(bbotk)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)
  library(ggplot2)

  # Branin global minimum of 0.397887 at (-pi, 12.275), (pi, 2.275), (9.42478, 2.475)
  # Eggholder global minimum of -959.6407 at (512, 404.2319)
  obfun = ObjectiveRFun$new(
    fun = function(xs) {
      a = 1
      b = 5.1 / (4 * (pi ^ 2))
      c = 5 / pi
      r = 6
      s = 10
      t = 1 / (8 * pi)
      #y = a * ((xs[["x2"]] - b * (xs[["x1"]] ^ 2) + c * xs[["x1"]] - r) ^ 2) +
      #  s * (1 - t) * cos(xs[["x1"]]) + s
      y = -(xs[["x2"]] + 47) * sin(sqrt(abs(xs[["x2"]] + (xs[["x1"]] / 2) + 47))) -
        xs[["x1"]] * sin(sqrt(abs(xs[["x1"]] - (xs[["x2"]] + 47))))
      list(y = y)
    },
    #domain = ParamSet$new(list(ParamDbl$new("x1", -5, 10), ParamDbl$new("x2", 0, 15))),
    domain = ParamSet$new(list(ParamDbl$new("x1", -512, 512), ParamDbl$new("x2", -512, 512))),
    codomain = ParamSet$new(list(ParamDbl$new("y", tags = "minimize"))),
    id = "Eggholder"
  )
  # obfun$eval_dt(data.table(x1 = c(-pi, pi, 9.42478), x2 = c(12.275, 2.275, 2.475)))
  # obfun$eval_dt(data.table(x1 = 512, x2 = 404.2319))

  #obfun_ = ObjectiveRFun$new(
  #  fun = function(xs) {
  #    a = 1
  #    b = 5.1 / (4 * (pi ^ 2))
  #    c = 5 / pi
  #    r = 6
  #    s = 10
  #    t = 1 / (8 * pi)
  #    y = a * ((xs[["x2"]] - b * (xs[["x1"]] ^ 2) + c * xs[["x1"]] - r) ^ 2) +
  #      s * (1 - t) * cos(xs[["x1"]]) + s
  #    list(y = - y)
  #  },
  #  domain = ParamSet$new(list(ParamDbl$new("x1", -5, 10), ParamDbl$new("x2", 0, 15))),
  #  codomain = ParamSet$new(list(ParamDbl$new("y", tags = "maximize"))),
  #  id = "Branin"
  #)

  terminator = trm("evals", n_evals = 150)

  instance = OptimInstanceSingleCrit$new(
    objective = obfun,
    terminator = terminator
  )

  #instance_ = OptimInstanceSingleCrit$new(
  #  objective = obfun_,
  #  terminator = terminator
  #)

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km", covtype = "gauss", optim.method = "gen", nugget.stability = 10^-8))
  acq_function_mes = AcqFunctionMES$new(surrogate = surrogate)
  acq_function_ei = AcqFunctionEI$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("grid_search", resolution = 2000, batch_size = 1000), trm("evals", n_evals = 1000))

  # FIXME: cloning should be handled in loops
  res = data.table()
  for (i in 1:10) {
    design = generate_design_random(instance$search_space, n = 10L)$data

    instance$archive$clear()
    instance$eval_batch(design)
    mes_res = bayesopt_soo(instance, acq_function_mes, acq_optimizer)$clone(deep = TRUE)
    instance$archive$clear()
    instance$eval_batch(design)
    ei_res = bayesopt_soo(instance, acq_function_ei, acq_optimizer)$clone(deep = TRUE)

    #instance_$archive$clear()
    #instance_$eval_batch(design)
    #mes_res_ = bayesopt_soo(instance_, acq_function_mes, acq_optimizer)$clone(deep = TRUE)
    #instance_$archive$clear()
    #instance_$eval_batch(design)
    #ei_res_ = bayesopt_soo(instance_, acq_function_ei, acq_optimizer)$clone(deep = TRUE)

    get_trace = function(archive, inverted = FALSE) {
      tmp = map_dtr(unique(archive$data$batch_nr), function(bn) archive$best(batch = 1:bn))
      if (inverted) {
        tmp$y = - tmp$y
      }
      tmp$iteration = seq_len(nrow(tmp))
      tmp$regret = tmp$y - -959.6407
      tmp
    }
    mes_resx = get_trace(mes_res)
    mes_resx$method = "mes"
    mes_resx$i = i
    ei_resx = get_trace(ei_res)
    ei_resx$method = "ei"
    ei_resx$i = i

    #mes_resx_ = get_trace(mes_res_, inverted = TRUE)
    #mes_resx_$method = "mes_"
    #mes_resx_$i = i
    #ei_resx_ = get_trace(ei_res_, inverted = TRUE)
    #ei_resx_$method = "ei__"
    #ei_resx_$i = i

    res = rbind(res, mes_resx, ei_resx, fill = TRUE)
  }

  dat = setNames(res[, mean(regret), by = .(iteration, method)], c("iteration", "method", "mean_regret"))
  dat$sd = res[, sd(regret), by = .(iteration, method)]$V1
  dat$se = dat$sd / sqrt(10)
  dat$upper = dat$mean_regret + dat$se
  dat$lower = dat$mean_regret - dat$se

  ggplot(aes(x = iteration, y = mean_regret, colour = method, fill = method), data = dat) +
    geom_line() +
    geom_ribbon(aes(x = iteration, max = log(upper), min = log(lower)), alpha = 0.25, colour = NA)
}
