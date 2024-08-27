callback_decay_lambda = callback_batch("mlr3mbo.decay_lambda",
  on_optimization_end = function(callback, context) {
    lambda = context$instance$objective$constants$get_values()[["lambda"]]
    context$instance$objective$constants$set_values("lambda" = lambda * 0.9)
  }
)

callback_sample_lambda = callback_batch("mlr3mbo.sample_lambda",
  on_optimization_begin = function(callback, context) {
    context$instance$objective$constants$set_values("lambda" = runif(1, min = 0.01, max = 10))
  }
)


load_callback_exponential_lambda_decay = function() {
  callback_async_tuning("mlr3mbo.exponential_lambda_decay",
    label = "Exponential Decay of Lambda",
    man = "mlr3mbo::mlr3mbo.exponential_lambda_decay",

    on_optimization_begin = function(callback, context) {
      self$state$rate = assert_number(self$state$rate, lower = 0, null.ok = TRUE) %??% 0.1
      self$state$period = assert_integer(self$state$period, lower = 1, null.ok = TRUE) %??% 25
      self$state$init_lambda = assert_number(self$state$init_lambda, lower = 0, null.ok = TRUE) %??% 1.96

      self$state$lambda_0 = rexp(1, 1 / self$state$init_lambda)
      self$state$t = 0
    },

    on_optimization_end = function(callback, context) {
      context$optimizer$
          t = self$state$t




          lambda = lambda_0 * exp(-self$state$rate * (t %% self$state$period))
          self$state$t = self$t + 1

    }
  )
}



# callback_exponential_lambda_decay


#         rate = p_dbl(lower = 0, default = 0.1),
#         period = p_int(lower = 1L, default = 25L),


#       lambda_0 = rexp(1, 1 / pv$lambda)
#       t = 0

#             lambda_0 = rexp(1, 1 / pv$lambda)
#       t = 0

#         if (pv$exponential_decay) {
#           lambda = lambda_0 * exp(-pv$rate * (t %% pv$period))
#           t = t + 1
#         } else {
#           lambda = pv$lambda
#         }
