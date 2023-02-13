library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

instance = readRDS("cd_instance.rds")
archive = instance$archive
data = instance$archive$data
cols = c(archive$cols_x, archive$cols_y, ".param")

for (i in seq_along(idx)) {
  print(data[idx[[i]], cols, with = FALSE])
  cat("\n")
  cat("\n")
}

png("hist1_k.png")
hist(data[1L, ]$raw_k[[1L]], main = "k, geom mean = 4.265765", xlab = "")
dev.off()

tmp = data[1L, ]$raw_mean_best[[1L]]
tmp[tmp > 1] = tmp[tmp > 1] / 100
png("hist1_target.png")
hist(tmp, main = "Mean target, mean = 0.9433329", xlab = "")
dev.off()

tmp = data[4L, ]$raw_mean_best[[1L]]
tmp[tmp > 1] = tmp[tmp > 1] / 100

png("hist2_k.png")
hist(data[5L, ]$raw_k[[1L]], main = "k, geom mean = 4.894954", xlab = "")
dev.off()

tmp = data[5L, ]$raw_mean_best[[1L]]
tmp[tmp > 1] = tmp[tmp > 1] / 100
png("hist2_target.png")
hist(tmp, main = "Mean target, mean = 0.9452976", xlab = "")
dev.off()




