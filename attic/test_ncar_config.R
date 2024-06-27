library(bbotk)
library(mlr3mbo)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3learners)

# learner = lrn("classif.rpart",
#   minsplit  = to_tune(2, 128, logscale = TRUE),
#   minbucket = to_tune(1, 64, logscale = TRUE),
#   cp        = to_tune(1e-04, 1e-1, logscale = TRUE)
# )

learner = lrn("classif.glmnet",
  alpha = to_tune(0, 1),
  lambda = to_tune(p_dbl(1e-4, 1e4, logscale = TRUE))
)

budget = 40L

instance = ti(
  task = tsk("sonar"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = budget),
)

init_design_size = 0.25 * budget

init_design = generate_design_lhs(instance$search_space, n = init_design_size)$data

instance$eval_batch(init_design)

lrn_mbo = lrn("regr.ranger_mbo",
  predict_type = "se",
  keep.inbag = TRUE,
  se.method = "simple",
  splitrule = "extratrees",
  num.random.splits = 1L,
  num.trees = 10L,
  replace = TRUE,
  sample.fraction = 1,
  min.node.size = 1,
  mtry.ratio = 1
)

surrogate = srlrn(as_learner(po("imputesample", affect_columns = selector_type("logical")) %>>%
  po("imputeoor", multiplier = 3, affect_columns = selector_type(c("integer", "numeric", "character", "factor", "ordered"))) %>>%
  po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
  lrn_mbo), catch_errors = TRUE)

acq_optimizer = acqo(
  optimizer = opt("focus_search", n_points = 1000L, maxit = 9L),
  terminator = trm("evals", n_evals = 20000L),
  catch_errors = FALSE
)

acq_function = acqf("cb", lambda = 1, check_values = FALSE)

tuner = tnr("mbo",
  loop_function = bayesopt_ego_log,
  surrogate = surrogate,
  acq_function = acq_function,
  acq_optimizer = acq_optimizer,
  args = list(random_interleave_iter = 4, init_design_size = init_design_size)
)

tuner$optimize(instance)
