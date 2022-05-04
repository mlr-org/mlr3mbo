library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3mbo)
library(ggplot2)
library(GGally)

x1 = readRDS("ac_instance_1.rds")
x2 = readRDS("ac_instance_2.rds")
x3 = readRDS("ac_instance_3.rds")

data = rbind(x1$archive$data, x2$archive$data, x3$archive$data)[, c(x1$archive$cols_x, x1$archive$cols_y), with = FALSE]
task = TaskRegr$new("mbo", backend = data, target = "mean_perf")
learner = default_surrogate(x1)$model
learner$param_set$values$regr.ranger.importance = "permutation"
learner$param_set$values$regr.ranger.num.trees = 2000L

rr = resample(task, learner, rsmp("cv", folds = 10L))
rr$aggregate(msr("regr.rsq"))
rr$aggregate(msr("regr.ktau"))

learner$train(task)
p = learner$predict(task)
task$data()[which.max(p$response), ]

imp = ranger::importance(learner$model$regr.ranger$model)
imp = data.table(importance = imp, feature = names(imp))
ggplot(aes(x = feature, y = importance), data = imp) +
  geom_bar(stat = "identity")

data[, mean_perf := p$response]
top5 = quantile(data$mean_perf, 0.95)
data[, top5 := as.factor(mean_perf >= top5)]

best = data[top5 == "TRUE", - "top5"]
best[, init := as.factor(init)]
best[, splitrule := as.factor(splitrule)]
best[, acqf := as.factor(acqf)]
best[, acqopt := as.factor(acqopt)]
best[, fs_behavior := as.factor(fs_behavior)]

ggparcoord(data = data, columns = c(1, 2, 4, 6, 8, 9),
           alphaLines = 0.2,
           groupColumn = "top5",
           order = "Outlying")
write.table(task$data(), "tmp.csv")

data = read.table("tmp.csv")
task = TaskRegr$new("mbo", backend = data, target = "mean_perf")
task = ppl("robustify", impute_missings = TRUE, factors_to_numeric = TRUE)$train(task)[[1L]]
data = task$data()
data[, random_interleave := as.integer(random_interleave)]

write.table(data[, - 1], "X.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(data[, 1], "Y.csv", sep = ",", row.names = FALSE, col.names = FALSE)

