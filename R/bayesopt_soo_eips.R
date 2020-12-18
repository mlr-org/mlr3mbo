if (FALSE) {
  set.seed(1)
  library(bbotk)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  obfun = ObjectiveRFun$new(
    fun = function(xs) list(y = sum(unlist(xs)^2), time = xs[[1]]),
    domain = ParamSet$new(list(ParamDbl$new("x", -5, 5))),
    codomain = ParamSet$new(list(
      ParamDbl$new("y", tags = "minimize"),
      ParamDbl$new("time", tags = "minimize") #FIXME: Use other tag e.g. budget, resource
    )),
    properties = "multi-crit",
    id = "test"
  )

  terminator = trm("evals", n_evals = 20)

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = terminator
  )

  surrogate = SurrogateMultiCritLearners$new(learners = replicate(2, lrn("regr.km")))
  acq_function = AcqFunctionEIPS$new(surrogate = surrogate)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000), trm("evals", n_evals = 1000))

  bayesopt_soo(instance, acq_function, acq_optimizer)

  data = instance$archive$data

  xgrid = generate_design_grid(instance$search_space, 200)$data
  preds = do.call(cbind, c(list(xgrid), acq_function$surrogate$predict(xgrid)))
  library(ggplot2)
  g = ggplot()
  g = g + geom_point(data = data, aes(x = x, y = y))
  g = g + geom_line(data = preds, aes(x = x, y = y.mean), col = "blue")
  g

  g = ggplot()
  g = g + geom_point(data = data, aes(x = x, y = time))
  g = g + geom_line(data = preds, aes(x = x, y = time.mean), col = "blue")
  g
}



