library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

dat = rbind(readRDS("results_yahpo.rds"), readRDS("results_yahpo_lfbo.rds"), readRDS("results_yahpo_mlr3mbo.rds"), readRDS("results_yahpo_lfbox.rds"))
dat[, cumbudget := cumsum(budget), by = .(method, scenario, instance, repl)]
dat[, cumbudget_scaled := cumbudget / max(cumbudget), by = .(method, scenario, instance, repl)]
dat[, normalized_regret := (target - min(target)) / (max(target) - min(target)), by = .(scenario, instance)]
dat[, incumbent := cummin(normalized_regret), by = .(method, scenario, instance, repl)]

get_incumbent_cumbudget = function(incumbent, cumbudget_scaled) {
  budgets = seq(0, 1, length.out = 101)
  map_dbl(budgets, function(budget) {
    ind = which(cumbudget_scaled <= budget)
    if (length(ind) == 0L) {
      max(incumbent)
    } else {
      min(incumbent[ind])
    }
  })
}

dat_budget = dat[, .(incumbent_budget = get_incumbent_cumbudget(incumbent, cumbudget_scaled), cumbudget_scaled = seq(0, 1, length.out = 101)), by = .(method, scenario, instance, repl)]

agg_budget = dat_budget[, .(mean = mean(incumbent_budget), se = sd(incumbent_budget) / sqrt(.N)), by = .(cumbudget_scaled, method, scenario, instance)]
#agg_budget[, method := factor(method, levels = c("random", "smac4hpo", "hb", "bohb", "dehb", "smac4mf", "optuna", "mlr3mbo", "mlr3mbo_default"), labels = c("Random", "SMAC", "HB", "BOHB", "DEHB", "SMAC-HB", "optuna", "mlr3mbo", "mlr3mbo_default"))]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = agg_budget[cumbudget_scaled > 0.10]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Fraction of Budget Used", y = "Mean Normalized Regret", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ scenario + instance, scales = "free", ncol = 5) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.5)))
ggsave("anytime.png", plot = g, device = "png", width = 15, height = 10)

overall_budget = agg_budget[, .(mean = mean(mean), se = sd(mean) / sqrt(.N)), by = .(method, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = overall_budget[cumbudget_scaled > 0.10]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.1) +
  labs(x = "Fraction of Budget Used", y = "Mean Normalized Regret", colour = "Optimizer", fill = "Optimizer") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.75)))
ggsave("anytime_average.png", plot = g, device = "png", width = 6, height = 4)

methods = unique(agg_budget$method)
ranks = map_dtr(unique(agg_budget$scenario), function(scenario_) {
  map_dtr(unique(agg_budget$instance), function(instance_) {
    map_dtr(unique(agg_budget$cumbudget_scaled), function(cumbudget_scaled_) {
      res = agg_budget[scenario == scenario_ & instance == instance_ & cumbudget_scaled == cumbudget_scaled_]
      if (nrow(res) == 0L) {
        return(data.table())
      }
      setorderv(res, "mean")
      data.table(rank = match(methods, res$method), method = methods, scenario = scenario_, instance = instance_, cumbudget_scaled = cumbudget_scaled_)
    })
  })
})

ranks_overall = ranks[, .(mean = mean(rank), se = sd(rank) / sqrt(.N)), by = .(method, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = ranks_overall[cumbudget_scaled > 0.10]) +
  geom_line() +
  geom_ribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Fraction of Budget Used", y = "Mean Rank", colour = "Optimizer", fill = "Optimizer") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.75)))
ggsave("anytime_average_rank.png", plot = g, device = "png", width = 6, height = 4)

library(scmamp)
best_agg = agg_budget[cumbudget_scaled == 0.25]
best_agg[, problem := paste0(scenario, "_", instance)]
tmp = - as.matrix(dcast(best_agg, problem ~ method, value.var = "mean")[, -1])
friedmanTest(tmp)
png("cd_025_mf.png", width = 6, height = 4, pointsize = 10, units = "in", res = 150)
plotCD(tmp, cex = 1)
dev.off()

best_agg = agg_budget[cumbudget_scaled == 1]
best_agg[, problem := paste0(scenario, "_", instance)]
tmp = - as.matrix(dcast(best_agg, problem ~ method, value.var = "mean")[, -1])
friedmanTest(tmp)
png("cd_1_mf.png", width = 6, height = 4, pointsize = 10, units = "in", res = 150)
plotCD(tmp, cex = 1)
dev.off()

