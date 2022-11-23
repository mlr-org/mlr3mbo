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
                       budget = rep(c(118, 90, 134, 110, 267, 118, 170), each = 4L))

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
