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
x4 = readRDS("ac_instance_4.rds")
x5 = readRDS("ac_instance_5.rds")

data = rbind(x1$archive$data, x2$archive$data, x3$archive$data, x4$archive$data, x5$archive$data)[, c(x1$archive$cols_x, x1$archive$cols_y), with = FALSE]
data[is.na(random_interleave_iter), random_interleave_iter := 0]
data[is.na(num.random.splits), num.random.splits := 0]
data[is.na(lambda), lambda := 0]
data[is.na(fs_behavior), fs_behavior := "none"]
chars = c("init", "splitrule", "acqf", "acqopt", "fs_behavior")
data[, (chars) := lapply(.SD, as.factor), .SDcols = chars]

lmx = lm(mean_perf ~ init * init_size_factor + random_interleave * random_interleave_iter + num.trees + splitrule * num.random.splits + acqf * lambda + acqopt_iter_factor * acqopt * fs_behavior, data = data)

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

write.table(task$data(), "tmp.csv")

data = read.table("tmp.csv")
task = TaskRegr$new("mbo", backend = data, target = "mean_perf")
task = ppl("robustify", impute_missings = TRUE, factors_to_numeric = TRUE)$train(task)[[1L]]
data = task$data()
data[, random_interleave := as.integer(random_interleave)]

write.table(data[, - 1], "X.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(data[, 1], "Y.csv", sep = ",", row.names = FALSE, col.names = FALSE)

