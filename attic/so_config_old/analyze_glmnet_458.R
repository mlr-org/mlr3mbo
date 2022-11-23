library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3mbo)

set.seed(1)

reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/yahpo_gym-4ygV7ggv/", required = TRUE)
source("helpers.R")
source("OptimizerChain.R")
library(yahpogym)
library(bbotk)
instance = make_optim_instance(data.table(scenario = "rbv2_glmnet", instance = "458", target = "acc", ndi = 3L, max_budget = 1, budget = 10^6, on_integer_scale = FALSE, minimize = FALSE))
xdt = generate_design_grid(instance$search_space, resolution = 100)$data
instance$eval_batch(xdt)
ref = copy(instance$archive$data)
breaks = quantile(ref$acc, c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999, 1))

instance = make_optim_instance(data.table(scenario = "rbv2_glmnet", instance = "458", target = "acc", ndi = 3L, max_budget = 1, budget = 90L, on_integer_scale = FALSE, minimize = FALSE))
d = instance$search_space$length
init_design_size = 4L * d
init_design = generate_design_lhs(instance$search_space, n = init_design_size)$data
instance$eval_batch(init_design)

random_interleave_iter = 0L

learner = lrn("regr.ranger_custom", num.trees = 10L, mtry.ratio = 1)
surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor", multiplier = 10) %>>% learner))

acq_function = AcqFunctionEI$new()

optimizer = OptimizerChain$new(list(opt("local_search", n_points = 100L), opt("random_search", batch_size = 1000L)), terminators = list(trm("evals", n_evals = 10010L), trm("evals", n_evals = 10000L)))
acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 20010L))

acq_optimizer$param_set$values$warmstart = TRUE
acq_optimizer$param_set$values$warmstart_size = "all"

bayesopt_ego(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)

mbo = copy(instance$archive$data)[, c("alpha", "num.impute.selected.cpo", "s", "acc")]
mbo[, method := "mbo"]
mbo[, repl := 1]
mbo[, iter := seq_len(.N)]

instance$archive$clear()
instance$eval_batch(init_design)

bayesopt_ego_log(instance, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer, random_interleave_iter = random_interleave_iter)

mbo_log = copy(instance$archive$data)[, c("alpha", "num.impute.selected.cpo", "s", "acc")]
mbo_log[, method := "mbo_log"]
mbo_log[, repl := 1]
mbo_log[, iter := seq_len(.N)]

glmnet_458_smac = readRDS("rbv2_glmnet_458_smac.rds")[repl == 2]
glmnet_458_smac[, method := "smac4hpo"]
glmnet_458_smac[, s := log(s)]

glmnet_458_mbo = readRDS("rbv2_glmnet_458_mlr3mbo_new_rf.rds")[repl == 1]
glmnet_458_mbo[, method := "mlr3mbo_new_rf"]

glmnet_458 = rbind(glmnet_458_smac, glmnet_458_mbo, mbo, mbo_log, fill = TRUE)

glmnet_458_best = rbind(glmnet_458[method == "smac4hpo"][which.max(acc), ],
  glmnet_458[method == "mlr3mbo_new_rf"][which.max(acc), ],
  glmnet_458[method == "mbo"][which.max(acc), ],
  glmnet_458[method == "mbo_log"][which.max(acc), ])

g1 = ggplot(data = ref, aes(x = alpha, y = s, z = acc)) +
  geom_point(aes(x = alpha, y = s, colour = iter, shape = method), data = glmnet_458[method %in% c("smac4hpo", "mbo", "mbo_log")], size = 3) +
  geom_contour(breaks = breaks) +
  geom_contour_filled(breaks = breaks, alpha = 0.1) +
  facet_grid(method ~ num.impute.selected.cpo) +
  theme(legend.position = "bottom")

g2 = ggplot(data = glmnet_458[method %in% c("smac4hpo", "mbo", "mbo_log") & iter > 12], aes(x = iter, y = cummax(acc), colour = method)) +
  geom_step()

instance_x = instance$clone(deep = TRUE)
instance$archive$data = glmnet_458[method == "mbo_log"]

learner = lrn("regr.ranger", mtry.ratio = 1, num.trees = 10L, se.method = "jack", min.node.size = 1L, splitrule = "extratrees", num.random.splits = 1L)
surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))
acq_function = AcqFunctionEI$new()
surrogate$archive = instance$archive
acq_function$surrogate = surrogate

surrogate$y_cols = "y_trafo"
acq_function$surrogate_max_to_min = 1

y = instance$archive$data[[instance$archive$cols_y]] * instance$objective_multiplicator
min_y = min(y) - 0.01 * diff(range(y))
max_y = max(y)
y_log = log((y - min_y) / (max_y - min_y))
instance$archive$data[, y_trafo := y_log]
acq_function$surrogate$update()
acq_function$y_best = min(y_log)  # manual update


acq_instance = OptimInstanceSingleCrit$new(objective = acq_function, search_space = acq_function$domain, terminator = trm("none"), check_values = FALSE, keep_evals = "all")
acq_instance$eval_batch(xdt)

sp = surrogate$predict(xdt)
acq_data = copy(acq_instance$archive$data)
acq_data = cbind(acq_data, sp)
acq_data[which.max(acq_ei), ]

acq_instance$archive$clear()

optimizer = OptimizerChain$new(list(opt("local_search", n_points = 100L), opt("random_search", batch_size = 1000L)), terminators = list(trm("evals", n_evals = 10010L), trm("evals", n_evals = 10000L)))
acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 20010L))

acq_optimizer$acq_function = acq_function
candidate = acq_optimizer$optimize()
acq_best = acq_data[which.max(acq_ei), ]

g3 = ggplot(data = acq_data, aes(x = alpha, y = s, z = mean)) +
  geom_contour() +
  geom_contour_filled() +
  facet_wrap(~ num.impute.selected.cpo) +
  theme(legend.position = "bottom")

g4 = ggplot(data = acq_data, aes(x = alpha, y = s, z = se)) +
  geom_contour() +
  geom_contour_filled() +
  facet_wrap(~ num.impute.selected.cpo) +
  theme(legend.position = "bottom")

g5 = ggplot(data = acq_data, aes(x = alpha, y = s, z = acq_ei)) +
  geom_contour() +
  geom_contour_filled() +
  geom_point(data = candidate, colour = "red", size = 2) +
  geom_point(data = acq_best, colour = "white", size = 2) +
  facet_wrap(~ num.impute.selected.cpo) +
  theme(legend.position = "bottom")

ggsave("/tmp/lol.png", plot = g5, width = 12, height = 3)

#library(mlr3)
#library(mlr3learners)
#library(mlr3pipelines)
#library(mlr3viz)
#
#tdat = readRDS("glmnet_458_mbo_data.rds")
#
#tdat[, num.impute.selected.cpo := as.factor(num.impute.selected.cpo)]
#task = TaskRegr$new("test", backend = tdat, target = "acc")
#resampling = rsmp("repeated_cv", folds = 10L)$instantiate(task)
#gp = as_learner(po("encode") %>>% po("fixfactors") %>>% lrn("regr.km", covtype = "matern3_2", optim.method = "gen"))
#gp2 = as_learner(po("encodeimpact") %>>% po("fixfactors") %>>% lrn("regr.km", covtype = "matern3_2", optim.method = "gen"))
#rp = as_learner(po("fixfactors") %>>% lrn("regr.rpart"))
#rf = as_learner(po("fixfactors") %>>% lrn("regr.ranger"))
#lm = as_learner(po("fixfactors") %>>% lrn("regr.lm"))
#kknn = as_learner(po("fixfactors") %>>% lrn("regr.kknn"))
#kknn1 = as_learner(po("fixfactors") %>>% lrn("regr.kknn", k = 1))
#kknn1$id = "kknn1"
#mars = as_learner(po("encode") %>>% po("fixfactors") %>>% lrn("regr.mars"))
#gam = as_learner(po("encode") %>>% po("fixfactors") %>>% lrn("regr.gam"))
#lm = as_learner(po("fixfactors") %>>% lrn("regr.lm"))
#learners = list(learner, gp, gp2, rp, rf, kknn, mars, gam, lm)
#bg = benchmark_grid(task, learners, resampling)
#b = benchmark(bg)
#scores = b$aggregate(msrs(c("regr.mse", "regr.mae", "regr.rsq", "regr.ktau")))
#scores_long = melt(scores, id.vars = "learner_id", measure.vars = c("regr.mse", "regr.mae", "regr.rsq", "regr.ktau"))
#
#ggplot(aes(x = learner_id, y = value), data = scores_long) +
#  geom_point(size = 3) +
#  facet_wrap(~ variable, scales = "free") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
#
#breaks = quantile(task$data()$acc, c(seq(0, 1, by = 0.1)))
#weights = as.numeric(cut(tdat$acc, breaks))
#weights[is.na(weights)] = 1
#task2 = TaskRegr$new("test", backend = cbind(tdat, weights), target = "acc")
#task2$col_roles$weight = "weights"
#task2$col_roles$feature = setdiff(task2$col_roles$feature, "weights")
#learners = list(rf, kknn, kknn1, lm, gp)
#
#map(learners, function(l) {
# l$train(task2)
#})
#
#diffs = map_dtr(1:100, function(r) {
#  print(r)
#  map_dtr(c(2^(5:13)), function(k) {
#    xdt_sample = ref[sample(.N, size = k, replace = FALSE), ]
#    best = xdt_sample[which.max(acc), ]
#    preds = map_dbl(learners, function(l) {
#     which.max(l$predict_newdata(xdt_sample)$response)
#    })
#    xdt_learner_p_best = xdt_sample[preds, ]
#    data.table(diff = best$acc - xdt_learner_p_best$acc, k = k, learner = map_chr(learners, "id"), repl = r)
#  })
#})
#
#mean_diffs = diffs[, .(mean_diff = mean(diff), se_diff = sd(diff) / sqrt(.N)), by = .(k, learner)]
#
#ggplot(aes(x = k, y = mean_diff, colour = learner), data = mean_diffs) +
#  geom_point() +
#  geom_errorbar(aes(ymin = mean_diff - se_diff, ymax = mean_diff + se_diff)) +
#  geom_line() +
#  theme_minimal() 

