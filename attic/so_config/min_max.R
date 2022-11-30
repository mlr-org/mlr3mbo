library(data.table)
library(bbotk)
library(mlr3misc)
library(paradox)
reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/yahpo_gym-4ygV7ggv/", required = TRUE)
library(reticulate)
library(yahpogym)


instances = data.table(scenario = rep(paste0("rbv2_", c("aknn", "glmnet", "ranger", "rpart", "super", "svm", "xgboost")), each = 4L),
                       instance = c("40499", "1476", "6", "12",
                                    "40979", "1501", "40966", "1478",
                                    "12", "458", "1510", "1515",
                                    "1478", "40979", "12", "28",
                                    "41164", "37", "1515", "1510",
                                    "1478", "1501", "40499", "40979",
                                    "40984", "40979", "40966", "28"),
                       target = "acc",
                       budget = rep(c(118L, 90L, 134L, 110L, 267L, 118L, 170L), each = 4L))

instances_so = data.table(scenario = rep(c("lcbench", "nb301", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_super", "rbv2_xgboost"), c(3L, 1L, 2L, 2L, 2L, 6L, 4L)),
                          instance = c("167168", "189873", "189906",
                                       "CIFAR10",
                                       "375", "458",
                                       "16", "42", 
                                       "14", "40499",
                                       "1053", "1457", "1063", "1479", "15", "1468",
                                       "12", "1501", "16", "40499"),
                          target = rep(c("val_accuracy", "val_accuracy", "acc", "acc", "acc", "acc", "acc"), c(3L, 1L, 2L, 2L, 2L, 6L, 4L)),
                          budget = rep(c(126L, 250L, 90L, 134L, 110L, 267L, 170L), c(3L, 1L, 2L, 2L, 2L, 6L, 4L)))

make_optim_instance = function(instance) {
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE, check_values = FALSE)
  budget = instance$budget
  optim_instance = OptimInstanceSingleCrit$new(objective, search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = 1000000L), check_values = FALSE)
  optim_instance
}

evaluate = function(instance) {
  optim_instance = make_optim_instance(instance)
  opt("random_search", batch_size = 10000L)$optimize(optim_instance)
  ys = optim_instance$archive$data[, optim_instance$archive$cols_y, with = FALSE][[1L]]
  data.table(min = min(ys), max = max(ys), mean = mean(ys), sd = sd(ys), ecdf = list(ecdf(ys)))
}

y_stats = map_dtr(seq_len(nrow(instances)), function(i) {
  evaluate(instances[i, ])
})

saveRDS(cbind(instances, y_stats), "instances.rds")

y_stats_so = map_dtr(seq_len(nrow(instances_so)), function(i) {
  evaluate(instances_so[i, ])
})

saveRDS(cbind(instances_so, y_stats_so), "instances_so.rds")

instances_so = readRDS("instances_so.rds")
smac = readRDS("results_yahpo.rds")[method == "smac4hpo"]
smac_best = smac[, .(best = min(target)), by = .(scenario, instance, repl)]

results = map_dtr(unique(instances_so$scenario), function(scenario_) {
  map_dtr(unique(instances_so$instance), function(instance_) {
    tmp = smac_best[scenario == scenario_ & instance == instance_]
    if (nrow(tmp) == 0) return(NULL)
    ecdf = instances_so[scenario == scenario_ & instance == instance_]$ecdf[[1L]](-tmp$best)
    diff = instances_so[scenario == scenario_ & instance == instance_]$max - (-tmp$best)
    if (scenario_ %in% c("lcbench", "nb301")) diff = diff / 100
    data.table(ecdf = ecdf, diff = diff, problem = paste0(scenario_, "_", instance_))
  })
})

library(ggplot2)
g = ggplot(aes(x = problem, y = ecdf), data = results) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g = ggplot(aes(x = problem, y = diff), data = results) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

