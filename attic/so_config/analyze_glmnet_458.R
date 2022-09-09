glmnet_458_smac = readRDS("rbv2_glmnet_458_smac.rds")
glmnet_458_smac[, method := "smac4hpo"]
glmnet_458_smac[, s := log(s)]

glmnet_458_mlr3mbo = readRDS("rbv2_glmnet_458_mlr3mbo_new_rf.rds")
glmnet_458_mlr3mbo[, method := "mlr3mbo"]
glmnet_458 = rbind(glmnet_458_smac, glmnet_458_mlr3mbo)

glmnet_458_best = map_dtr(1:30, function(r) {
  rbind(glmnet_458[method == "smac4hpo" & repl == r, ][which.max(acc), ],
        glmnet_458[method == "mlr3mbo" & repl == r, ][which.max(acc), ])
})

reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/yahpo_gym-4ygV7ggv/", required = TRUE)
source("helpers.R")
library(yahpogym)
library(bbotk)
instance = make_optim_instance(data.table(scenario = "rbv2_glmnet", instance = "458", target = "acc", ndi = 3L, max_budget = 1, budget = 10^6, on_integer_scale = FALSE, minimize = FALSE))
xdt = generate_design_grid(instance$search_space, resolution = 100)$data
instance$eval_batch(xdt)
ref = instance$archive$data
breaks = quantile(ref$acc, c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999, 1))

r = 2

g = ggplot(data = ref, aes(x = alpha, y = s, z = acc)) +
  geom_point(aes(x = alpha, y = s, colour = iter, shape = method), data = glmnet_458[repl == r], size = 3) +
  geom_contour(breaks = breaks) +
  geom_contour_filled(breaks = breaks, alpha = 0.1) +
  facet_grid(method ~ num.impute.selected.cpo) +
  theme(legend.position = "bottom")

instance_x = instance$clone(deep = TRUE)
instance$archive$data = glmnet_458[method == "mlr3mbo" & repl == 1]

learner = lrn("regr.ranger_custom")
surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner))

acq_function = AcqFunctionEI$new()
surrogate$archive = instance$archive
acq_function$surrogate = surrogate
acq_function$surrogate$update()
acq_function$update()

acq_instance = OptimInstanceSingleCrit$new(objective = acq_function, search_space = acq_function$domain, terminator = trm("none"), check_values = FALSE, keep_evals = "all")
acq_instance$eval_batch(xdt)

acq_data = copy(acq_instance$archive$data)
acq_data = cbind(acq_data, sp)
breaks = quantile(acq_data$mean, c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999, 1))

acq_instance$archive$clear()

acq_budget = 20000L
  
acq_optimizer = {
  n_repeats = 2L
  maxit = 9L
  batch_size = ceiling((acq_budget / n_repeats) / (1 + maxit))
  AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = acq_budget))
}

acq_optimizer$acq_function = acq_function
candidate = acq_instance$archive$best()

g3 = ggplot(data = acq_data, aes(x = alpha, y = s, z = mean)) +
  geom_contour(breaks = breaks) +
  geom_contour_filled(breaks = breaks) +
  facet_wrap(~ num.impute.selected.cpo) +
  theme(legend.position = "bottom")

saveRDS(instance$archive$data[, c(instance$archive$cols_x, instance$archive$cols_y), with = FALSE], "glmnet_458_mbo_data.rds")

library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3viz)


tdat = readRDS("glmnet_458_mbo_data.rds")

tdat[, num.impute.selected.cpo := as.factor(num.impute.selected.cpo)]
task = TaskRegr$new("test", backend = tdat, target = "acc")
resampling = rsmp("repeated_cv", folds = 10L)$instantiate(task)
gp = as_learner(po("encode") %>>% po("fixfactors") %>>% lrn("regr.km", covtype = "matern3_2", optim.method = "gen"))
gp2 = as_learner(po("encodeimpact") %>>% po("fixfactors") %>>% lrn("regr.km", covtype = "matern3_2", optim.method = "gen"))
rp = as_learner(po("fixfactors") %>>% lrn("regr.rpart"))
rf = as_learner(po("fixfactors") %>>% lrn("regr.ranger"))
lm = as_learner(po("fixfactors") %>>% lrn("regr.lm"))
kknn = as_learner(po("fixfactors") %>>% lrn("regr.kknn"))
kknn1 = as_learner(po("fixfactors") %>>% lrn("regr.kknn", k = 1))
kknn1$id = "kknn1"
mars = as_learner(po("encode") %>>% po("fixfactors") %>>% lrn("regr.mars"))
gam = as_learner(po("encode") %>>% po("fixfactors") %>>% lrn("regr.gam"))
lm = as_learner(po("fixfactors") %>>% lrn("regr.lm"))
learners = list(learner, gp, gp2, rp, rf, kknn, mars, gam, lm)
bg = benchmark_grid(task, learners, resampling)
b = benchmark(bg)
scores = b$aggregate(msrs(c("regr.mse", "regr.mae", "regr.rsq", "regr.ktau")))
scores_long = melt(scores, id.vars = "learner_id", measure.vars = c("regr.mse", "regr.mae", "regr.rsq", "regr.ktau"))

ggplot(aes(x = learner_id, y = value), data = scores_long) +
  geom_point(size = 3) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

breaks = quantile(task$data()$acc, c(seq(0, 1, by = 0.1)))
weights = as.numeric(cut(tdat$acc, breaks))
weights[is.na(weights)] = 1
task2 = TaskRegr$new("test", backend = cbind(tdat, weights), target = "acc")
task2$col_roles$weight = "weights"
task2$col_roles$feature = setdiff(task2$col_roles$feature, "weights")
learners = list(rf, kknn, kknn1, lm, gp)

map(learners, function(l) {
 l$train(task2)
})

diffs = map_dtr(1:100, function(r) {
  print(r)
  map_dtr(c(2^(5:13)), function(k) {
    xdt_sample = ref[sample(.N, size = k, replace = FALSE), ]
    best = xdt_sample[which.max(acc), ]
    preds = map_dbl(learners, function(l) {
     which.max(l$predict_newdata(xdt_sample)$response)
    })
    xdt_learner_p_best = xdt_sample[preds, ]
    data.table(diff = best$acc - xdt_learner_p_best$acc, k = k, learner = map_chr(learners, "id"), repl = r)
  })
})

mean_diffs = diffs[, .(mean_diff = mean(diff), se_diff = sd(diff) / sqrt(.N)), by = .(k, learner)]

ggplot(aes(x = k, y = mean_diff, colour = learner), data = mean_diffs) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_diff - se_diff, ymax = mean_diff + se_diff)) +
  geom_line() +
  theme_minimal() 

