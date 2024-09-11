#' @title Sample Lambda from an Uniform Distribution
#'
#' @name mlr3mbo.sample_lambda
#'
#' @description
#' This [CallbackAsyncTuning] samples the lambda parameter of the confidence bounds acquisition function.
#' The lambda value is drawn from a uniform distribution with `min` and `max` as bounds.
#'
#' @param min_lambda (`numeric(1)`)\cr
#' Minimum value of the lambda parameter.
#' Defaults to `0.01`.
#' @param max_lambda (`numeric(1)`)\cr
#' Maximum value of the lambda parameter.
#' Defaults to `10`.
#'
#' @examples
#' clbk("mlr3mbo.sample_lambda", min_lambda = 0.01, max_lambda = 10)
NULL

load_callback_sample_lambda_uniform = function() {
  callback_async_tuning("mlr3mbo.sample_lambda_uniform",
    label = "Sample Lambda Uniform",
    man = "mlr3mbo::mlr3mbo.sample_lambda_uniform",

    on_optimization_begin = function(callback, context) {
      assert_class(context$optimizer$acq_function, "AcqFunctionCB")
      callback$state$min_lambda = assert_number(callback$state$min_lambda, lower = 0, null.ok = TRUE) %??% 0.01
      callback$state$max_lambda = assert_number(callback$state$max_lambda, lower = 0, null.ok = TRUE) %??% 10
    },

    on_worker_begin = function(callback, context) {
      callback$state$lambda = runif(1, min = callback$state$min_lambda, max = callback$state$max_lambda)
      context$optimizer$acq_function$constants$set_values(lambda = callback$state$lambda)
    },

    on_optimizer_after_eval = function(callback, context) {
      context$ys = c(context$ys, list(lambda = callback$state$lambda))
    }
  )
}

callbacks[["mlr3mbo.sample_lambda_uniform"]] = load_callback_sample_lambda_uniform

#' @title Sample Lambda from an Exponential Distribution
#'
#' @name mlr3mbo.sample_lambda_exponential
#'
#' @description
#' This [CallbackAsyncTuning] samples the lambda parameter of the confidence bounds acquisition function.
#' The lambda value is drawn from an exponential distribution with rate `1 / lambda`.
#'
#' @examples
#' clbk("mlr3mbo.sample_lambda_exponential")
NULL

load_callback_sample_lambda_exponential = function() {
  callback_async_tuning("mlr3mbo.sample_lambda_exponential",
    label = "Sample Lambda Exponential",
    man = "mlr3mbo::mlr3mbo.sample_lambda_exponential",

    on_optimization_begin = function(callback, context) {
      assert_class(context$optimizer$acq_function, "AcqFunctionCB")
    },

    on_worker_begin = function(callback, context) {
      lambda = context$optimizer$acq_function$constants$values$lambda
      callback$state$lambda = rexp(1, 1 / lambda)
      context$optimizer$acq_function$constants$set_values(lambda = callback$state$lambda)
    },

    on_optimizer_after_eval = function(callback, context) {
      context$ys = c(context$ys, list(lambda = callback$state$lambda))
    }
  )
}

callbacks[["mlr3mbo.sample_lambda_exponential"]] = load_callback_sample_lambda_exponential

#' @title Exponential Decay of Lambda
#'
#' @name mlr3mbo.lambda_decay
#'
#' @description
#' This [CallbackAsyncTuning] decays the lambda parameter of the confidence bounds acquisition function.
#' The initial lambda value is drawn from an exponential distribution with rate `1 / lambda`.
#' The lambda value is updated after each evaluation by the formula `lambda * exp(-rate * (t %% period))`.
#'
#' @param rate (`numeric(1)`)\cr
#' Rate of the exponential decay.
#' Defaults to `0.1`.
#' @param period (`integer(1)`)\cr
#' Period of the exponential decay.
#' Defaults to `25`.
#'
#' @examples
#' clbk("mlr3mbo.exponential_lambda_decay", rate = 0.1, period = 25)
NULL

load_callback_exponential_lambda_decay = function() {
  callback_async_tuning("mlr3mbo.exponential_lambda_decay",
    label = "Exponential Decay of Lambda",
    man = "mlr3mbo::mlr3mbo.exponential_lambda_decay",

    on_optimization_begin = function(callback, context) {
      assert_class(context$optimizer$acq_function, "AcqFunctionCB")
      callback$state$rate = assert_number(callback$state$rate, lower = 0, null.ok = TRUE) %??% 0.1
      callback$state$period = assert_integer(callback$state$period, lower = 1, null.ok = TRUE) %??% 25
      callback$state$t = 0
    },

    on_worker_begin = function(callback, context) {
      lambda = context$optimizer$acq_function$constants$values$lambda
      callback$state$lambda_0 = rexp(1, 1 / lambda)
      context$optimizer$acq_function$constants$set_values(lambda = callback$state$lambda_0)
    },

    on_optimizer_after_eval = function(callback, context) {
      if (!is.null(context$extra)) { # skip initial design
        lambda_0 = callback$state$lambda_0
        t = callback$state$t
        lambda = lambda_0 * exp(-callback$state$rate * (t %% callback$state$period))
        callback$state$t = t + 1
        context$optimizer$acq_function$constants$set_values(lambda = lambda)
        context$ys = c(context$ys, list(lambda_0 = callback$state$lambda_0, lambda = lambda))
      }
    }
  )
}

callbacks[["mlr3mbo.exponential_lambda_decay"]] = load_callback_exponential_lambda_decay

#' @title Epsilon Decay
#'
#' @name mlr3mbo.sample_epsilon
#'
#' @description
#' This [CallbackAsyncTuning] decays the epsilon parameter of the expected improvement acquisition function.
#'
#' @param rate (`numeric(1)`)\cr
#' Rate of the exponential decay.
#' Defaults to `0.1`.
#' @param period (`integer(1)`)\cr
#' Period of the exponential decay.
#' Defaults to `25`.
#' @examples
#' clbk("mlr3mbo.sample_epsilon", rate = 0.1, period = 25)
NULL

load_callback_epsilon_decay = function() {
  callback_async_tuning("mlr3mbo.epsilon_decay",
    label = "Episilon Decay",
    man = "mlr3mbo::mlr3mbo.epsilon_decay",

    on_optimization_begin = function(callback, context) {
      assert_class(context$optimizer$acq_function, "AcqFunctionEI")
      epsilon = context$optimizer$acq_function$constants$values$epsilon
      callback$state$epsilon_0 = epsilon
      callback$state$epsilon = epsilon
      callback$state$rate = assert_number(callback$state$rate, lower = 0, null.ok = TRUE) %??% 0.1
      callback$state$period = assert_number(callback$state$period, lower = 1, null.ok = TRUE) %??% 25
      callback$state$t = 0
    },

    on_optimizer_after_eval = function(callback, context) {
      if (!is.null(context$extra)) { # skip initial design
        t = callback$state$t
        epsilon_0 = callback$state$epsilon_0
        epsilon = epsilon_0 * exp(-callback$state$rate * (t %% callback$state$period))
        callback$state$t = t + 1
        context$optimizer$acq_function$constants$set_values(epsilon = epsilon)
        context$ys = c(context$ys, list(epsilon_0 = epsilon_0, epsilon = epsilon))
      }
    }
  )
}

callbacks[["mlr3mbo.epsilon_decay"]] = load_callback_epsilon_decay
