context("OptimizerMbo")

library(mlr3learners)

test_that("OptimizerMbo works", {

  obfun = OBJ_1D

  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  design = generate_design_lhs(PS_1D, 4)$data
  design = cbind(design, obfun$eval_dt(design))

  optim = OptimizerMbo$new(
    loop_function = bayesop_soo,
    acq_function = AcqFunctionEI$new(surrogate = surrogate),
    acq_optimizer = AcqOptimizerRandomSearch$new()
  )

  instance = OptimInstanceSingleCrit$new(
    objective = obfun, 
    terminator = trm("evals", n_evals = 10),
    search_space = PS_1D
  )
  
  optim$optimize(instance)

  opdf = instance$archive$data()
  expect_data_table(opdf, any.missing = FALSE, nrows = 10)
  expect_equal(instance$result$y, 0, tolerance = 0.1)
})


test_that("OptimizerMbo works with different settings", {
  
  # defince common settings
  obfun = OBJ_2D
  surrogate = SurrogateSingleCritLearner$new(learner = lrn("regr.km"))
  design = generate_design_lhs(PS_2D, 6)$data
  design = cbind(design, obfun$eval_dt(design))
  term = trm("evals", n_evals = 12)

  # define combinations
  loop_functions = list(
    soo = list(fun = bayesop_soo), 
    mpcl = list(fun = bayesop_mpcl, args = list(liar = mean, q = 2))
  )
  acq_functions = list(
    #aei = AcqFunctionAEI$new(surrogate), #FIXME
    cb = AcqFunctionCB$new(surrogate), 
    ei = AcqFunctionEI$new(surrogate)
  )
  acq_optimizers = list(
    rs = AcqOptimizerRandomSearch$new(),
    from_optim = AcqOptimizerFromOptimizer$new(
      opt("random_search", batch_size = 1000), 
      trm("evals", n_evals = 1000)
    )
  )

  combinations = cross_join(list(
    loop_function = loop_functions, 
    acq_function = acq_functions, 
    acq_optimizer = acq_optimizers
  ), sorted = FALSE)

  for (i in seq_row(combinations)) {
    mbo_fun = combinations[i,"loop_function"][[1]][[1]]$fun
    args = combinations[i,"loop_function"][[1]][[1]]$args
    acq_fun = combinations[i,"acq_function"][[1]][[1]]
    acq_opt = combinations[i,"acq_optimizer"][[1]][[1]]
    optim = OptimizerMbo$new(
      loop_function = mbo_fun,
      acq_function = acq_fun,
      acq_optimizer = acq_opt,
      args = args
    )

    instance = OptimInstanceSingleCrit$new(
      objective = obfun, 
      terminator = term,
      search_space = PS_2D
    )
    
    optim$optimize(instance)

    opdf = instance$archive$data()
    expect_data_table(opdf, any.missing = FALSE, nrows = 12)
    expect_equal(instance$result$y, 0, tolerance = 0.2)
  }
})

