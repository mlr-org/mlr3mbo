library(data.table)
library(mlr3misc)

fs_average = readRDS("results_yahpo_fs_average.rds")
fs = readRDS("results_yahpo_fs.rds")[scenario == "lcbench" & instance == 167152]


get_k = function(best, scenario_, instance_, budget_) {
  # assumes maximization
  if (best > max(fs_average[scenario == scenario_ & instance == instance_][["mean_best"]])) {
    extrapolate = TRUE
    k = fs_extrapolation[scenario == scenario_ & instance == instance_][["model"]][[1L]](best)
  } else {
    extrapolate = FALSE
    k = min(fs_average[scenario == scenario_ & instance == instance_ & mean_best >= best]$iter)  # min k so that mean_best_fs[k] >= best_mbo[final]
  }
  k = k / budget_  # sample efficiency compared to fs
  attr(k, "extrapolate") = extrapolate
  k
}

k = get_k(97.543, scenario_ = "lcbench", instance_ = 167152, budget_ = 126)

perfs = 97.543 + rnorm(100L, mean = 0, sd = 0.01)
ks = map_dbl(perfs, function(perf) get_k(perf, scenario_ = "lcbench", instance_ = 167152, budget_ = 126))

png("k_noise.png")
plot(perfs, ks, xlab = "Validation Accuracy", ylab = "k")
dev.off()

ks_alt = map_dtr(perfs, function(perf) {
  tmp = map_dbl(1:30, function(i) {
    min(fs[scenario == "lcbench" & instance == 167152 & repl == i & best >= perf]$iter) / 126
   })
  data.table(mean = mean(tmp), geom = exp(mean(log(tmp))))
})

png("k_noise_mean.png")
plot(perfs, ks_alt$mean, xlab = "Validation Accuracy", ylab = "k")
dev.off()

png("k_noise_geom.png")
plot(perfs, ks_alt$geom, xlab = "Validation Accuracy", ylab = "k")
dev.off()

