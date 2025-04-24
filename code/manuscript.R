### tables
rm(list = ls())


library(tidyverse)
library(gtsummary)
library(kableExtra)
library(glue)

result = read_rds(here::here("data", "all_fprint_res.rds"))
result100 = read_rds(here::here("data", "all_fprint_100s.rds"))
result_all = read_rds(here::here("data", "all_fprint_folds.rds"))
result_ov = read_rds(here::here("data", "all_fprint_res_ov.rds"))

result =
  result %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random"),
         type = case_when(
           sub(".*\\d", "", name) == "xgb" ~ "XGBoost",
           sub(".*\\d", "", name) == "long" ~ "Long",
           sub(".*\\d", "", name) == "lasso" ~ "Lasso",
           sub(".*\\d", "", name) == "rf" ~ "Random Forest",
           sub(".*\\d", "", name) == "fnl" ~ "Linear SoFR",
           sub(".*\\d", "", name) == "nlfnl" ~ "Nonlinear SoFR",
           .default = "Logistic"))
result_all =
  result_all %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random"),
         type = case_when(
           sub(".*\\d", "", name) == "xgb" ~ "XGBoost",
           sub(".*\\d", "", name) == "long" ~ "Long",
           sub(".*\\d", "", name) == "lasso" ~ "Lasso",
           sub(".*\\d", "", name) == "rf" ~ "Random Forest",
           sub(".*\\d", "", name) == "fnl" ~ "Linear SoFR",
           sub(".*\\d", "", name) == "nlfnl" ~ "Nonlinear SoFR",
           .default = "Logistic"))

result100 = result %>% filter(n_sub == 100)

result100 %>%
  filter(type != "Long") %>%
  select(temporal, type, rank1_min, rank1_sd, rank1_max, rank1_mean,
         rank5_min, rank5_max, rank5_mean, rank5_sd) %>%
  mutate(across(contains("rank"), ~round(.x, 1))) %>%
  mutate(r1 = glue('{rank1_mean} ({rank1_sd}) [{rank1_min},{rank1_max}]'),
         r5 = glue('{rank5_mean} ({rank5_sd}) [{rank5_min},{rank5_max}]')) %>%
  group_by(temporal) %>%
  arrange(desc(rank1_mean), .by_group = TRUE) %>%
  select(temporal, type, r1, r5) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

result_all %>%
  filter(n %in% c(100, 500) & type != "Long") %>%
  group_by(temporal, type, n) %>%
  summarize(across(c(rank1, rank5), list(med = median, min = min, max = max)), .groups = "drop") %>%
  group_by(temporal) %>%
  arrange(desc(rank1_med), .by_group = TRUE) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = glue('{rank1_med} [{rank1_min},{rank1_max}]'),
         r5 = glue('{rank5_med} [{rank5_min},{rank5_max}]')) %>%
  select(temporal, type, n, r1, r5) %>%
  pivot_wider(names_from = n, values_from = c(r1, r5)) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

result_all %>%
  filter(n == 100) %>%
  group_by(temporal) %>%
  ggplot(aes(x = fct_reorder(type, rank1, median, .desc = TRUE), y = rank1)) +
  geom_line(aes(group = fold), alpha= .2, linewidth = .2) +
  geom_boxplot(outlier.shape = NA,
               aes(color = fct_reorder(type, rank1, median, .desc = TRUE))) +
  geom_jitter(width = .2, alpha = .2, size = .5) +
  facet_wrap(.~temporal, scales = "free_x") +
  theme_light() +
  theme(legend.position = "none")+
  labs(x = "Model", y = "Rank 1 Accuracy")

result_all %>%
  filter(n %in% c(100, 500) & type != "Long") %>%
  mutate(n = factor(n, labels = c("n = 100", "n = 500"))) %>%
  group_by(temporal) %>%
  ggplot(aes(x = fct_reorder(type, rank1, median, .desc = TRUE), y = rank1)) +
  # geom_line(aes(group = fold), alpha= .2, linewidth = .2) +
  geom_boxplot(outlier.shape = NA,
               aes(color = fct_reorder(type, rank1, median, .desc = TRUE))) +
  geom_jitter(width = .2, alpha = .2, size = .5) +
  facet_wrap(n~temporal, scales = "free_x") +
  theme_light() +
  theme(legend.position = "none")+
  labs(x = "Model", y = "Rank 1 Accuracy")

p = result %>%
  filter(n_sub %in% c(100, 500, 1000, 2500, 5000, 10000, 11225, 13367)) %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random"),
         type = case_when(
           sub(".*\\d", "", name) == "xgb" ~ "XGBoost",
           sub(".*\\d", "", name) == "long" ~ "Long",
           sub(".*\\d", "", name) == "lasso" ~ "Lasso",
           sub(".*\\d", "", name) == "rf" ~ "Random Forest",
           sub(".*\\d", "", name) == "fnl" ~ "Linear SoFR",
           sub(".*\\d", "", name) == "nlfnl" ~ "Nonlinear SoFR",
           .default = "Logistic")) %>%
  filter(type == "Logistic") %>%
  select(n_sub, rank1_median, rank5_median, rank1pct_median, rank5pct_median, type, temporal) %>%
  pivot_longer(cols = contains("rank")) %>%
  # mutate(name = factor(name, labels = c("Rank 1", "Rank 5"))) %>%
  ggplot(aes(x = n_sub, y = value, color = name, group = name)) +
  geom_jitter(width = 25, size = 2)  +
  geom_line(linewidth = 1.1) +
  facet_grid(.~temporal, scales = "free_x") +
  scale_x_continuous(breaks=c(100, 500, 1000, 2500, 5000, 10000, 11225, 13367),
                     labels = c(1, 5, 10, 25, 50, 100, 1122, 1336)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = c(.8, .8),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  labs(x = "Number Subjects (x100)", y = "Median Accuracy") +
  scale_color_manual(values = c("#FF7F00FF", "#FFBF7FFF", "#654CFFFF", "#CCBFFFFF"),
                     labels = c("Rank 1", "Rank 1%", "Rank 5", "Rank 5%"), name = "Metric")  +
  geom_text_repel(data = . %>%
                     filter(n_sub %in% c(100, 11225, 13367)),
                            aes(label = round(value, 0)), size = 2.5)
png(here::here("manuscript", "figs", "acc_pct.png"), width = 8, height = 6, res = 500, units = "in")
p
dev.off()

p = result  %>%
  filter(type == "Long" | type == "Logistic", temporal == "Random") %>%
  filter(n_sub %in% c(100, 1000, 2500, 5000, 10000, 10129)) %>%
  select(rank1_median, rank5_median, type, n_sub) %>%
  pivot_longer(cols = c(rank1_median, rank5_median)) %>%
  ggplot(aes(x = n_sub, y = value, color = type, linetype = name)) +
  # facet_grid(.~name) +
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("ggthemes::colorblind", direction = 1,
                          labels = c("3 minutes", "6 minutes"), name = "Amount of Data") +
  labs(x = "Number Subjects (x100)", y = "Accuracy") +
  scale_linetype_discrete(name = "Metric", labels = c("Rank 1", "Rank 5")) +
  theme(legend.position = c(0.8, 0.7),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  scale_x_continuous(breaks=c(100,  1000, 2500, 5000, 10000),
                     labels = c(1, 10, 25, 50, 100)) +
  scale_y_continuous(breaks=seq(0,100, 10), limits = c(0,100)) +
  guides(color = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1))
png(here::here("manuscript", "figs", "acc_long.png"), width = 8, height = 6, res = 500, units = "in")
p
dev.off()

result_all %>%
  filter(n %in% c(100, 500) & type != "Long") %>%
  mutate(n = factor(n, labels = c("n = 100", "n = 500"))) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "rank_type", values_to = "rank") %>%
  ggplot(aes(x = rank_type, y = rank, color = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .2, alpha = .2, size = .5) +
  facet_wrap(n~temporal, scales = "free_x") +
  theme_light() +
  theme(legend.position = "none")+
  labs(x = "Model", y = "Rank 1 Accuracy")

p = result_all %>%
  filter(n %in% c(100, 500) & type != "Long") %>%
  mutate(order2 = fct_reorder(type, rank1, median, .desc = TRUE)) %>%
  mutate(n = factor(n, labels = c("Subset size = 100", "Subset size = 500"))) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "measure", values_to = "rank") %>%
  ggplot(aes(x = measure, y = rank, color = order2)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge()) +
  geom_point(pch = 21, size = .3, alpha = .5, position = position_jitterdodge()) +
  facet_grid(temporal~n) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
  labs(x = "", y = "Accuracy") +
  scale_color_paletteer_d("ggthemes::colorblind", direction = -1, name = "")  +
  scale_x_discrete(labels = c("Rank 1", "Rank 5")) +
  guides(color = guide_legend(nrow = 1))
png(here::here("manuscript", "figs", "acc100_500.png"), width = 8, height = 6, res = 500, units = "in")
p
dev.off()

result_folds %>%
  filter(n == 100) %>%
  filter(type != "Long") %>%
  group_by(temporal) %>%
  ggplot(aes(x = fct_reorder(type, rank1, median, .desc = TRUE), y = rank1)) +
  # geom_line(aes(group = fold), alpha= .2, linewidth = .2) +
  geom_boxplot(outlier.shape = NA,
               aes(color = fct_reorder(type, rank1, median, .desc = TRUE))) +
  geom_jitter(width = .2, alpha = .2, size = .5) +
  facet_wrap(.~temporal, scales = "free_x") +
  theme_light() +
  theme(legend.position = "none")+
  labs(x = "Model", y = "Rank 1 Accuracy") +
  geom_label(data = lab_df, aes(x = type, y = med, label = round(med, 2)), nudge_y = 5, nudge_x = .25,
             size = 3) +
  scale_color_paletteer_d("colorBlindness::paletteMartin")

result_all %>%
  filter(n == 1000) %>%
  group_by(temporal) %>%
  ggplot(aes(x = fct_reorder(type, rank1, median, .desc = TRUE), y = rank1)) +
  geom_line(aes(group = fold), alpha= .2, linewidth = .2) +
  geom_boxplot(outlier.shape = NA,
               aes(color = fct_reorder(type, rank1, median, .desc = TRUE))) +
  geom_jitter(width = .2, alpha = .2, size = .5) +
  facet_wrap(.~temporal, scales = "free_x") +
  theme_light() +
  theme(legend.position = "none")+
  labs(x = "Model", y = "Rank 1 Accuracy")

## look at xgboost folds that do terribly

result_all %>%
  filter(n == 100 & type == "XGBoost")  %>%
  arrange(rank1)

# 108, 114, 127

def =
  result_all %>%
  filter(type == "Logistic") %>%
  group_by(n_tar, temporal) %>%
  summarize(across(c(rank1, rank5),  ~median(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(factor = "default",
         factor2 = 1 / n_tar)  %>%
  rename(rank1_median = rank1,
         rank5_median = rank5,
         n_sub = n_tar)  %>%
  filter(n_sub %in% c(100, 500, 1000))

p = result_ov %>%
  filter(factor < 1) %>%
  mutate(factor = as.character(factor)) %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random")) %>%
  bind_rows(def) %>%
  filter(n_sub %in% c(100, 500, 1000)) %>%
  pivot_longer(cols = c(rank1_median, rank5_median), names_to = "metric", values_to = "value") %>%
  mutate(ovsamp = factor(factor),
         metric = factor(metric, labels = c("Rank 1", "Rank 5"))) %>%
  ggplot(aes(x = n_sub, y = value, color = ovsamp)) +
  geom_point(size = 2) +
  geom_line(aes(group = ovsamp), linewidth = .9) +
  facet_grid(temporal~metric, scales = "free_y") +
  theme_light() +
  scale_color_manual(values = c("#290AD8FF", "#3FA0FFFF", "#FFE099FF",
  "#FFAD72FF", "#F76D5EFF", "grey"), name = "Proportion of data\ncomprised by outcome")+
  labs(x = "Subset Size", y = "Median Accuracy") +
  scale_x_continuous(breaks=c(100, 500, 1000), labels = c(100, 500, 1000))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

# geom_label(data = def,
  #            aes(x = n_sub, y = rank1_mean, label = factor2), nudge_y = 0, nudge_x = .0,
  #            size = 2, inherit.aes = FALSE, col = "grey")

# paletteer::paletteer_d("colorBlindness::Blue2DarkRed12Steps")

png(here::here("manuscript", "figs", "oversamp_acc.png"), width = 8, height = 6, res = 500, units = "in")
p
dev.off()

# table
result_ov %>%
  filter(factor < 1) %>%
  mutate(factor = as.character(factor)) %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random")) %>%
  bind_rows(def) %>%
  filter(n_sub %in% c(100, 500, 1000)) %>%
  # pivot_longer(cols = c(rank1_median, rank5_median), names_to = "metric", values_to = "value") %>%
  mutate(ovsamp = factor(factor)) %>%
  select(ovsamp, n_sub, rank1_median, rank5_median, temporal) %>%
  pivot_wider(names_from = n_sub, values_from = c(rank1_median, rank5_median)) %>%
  group_by(temporal) %>%
  arrange(temporal) %>%
  select(temporal, ovsamp, rank1_median_100, rank1_median_500, rank1_median_1000,
         rank5_median_100, rank5_median_500, rank5_median_1000) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

