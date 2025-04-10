library(tidyverse)

result = read_rds(here::here("data", "all_fprint_res.rds"))

result_folds  = read_rds(here::here("data", "all_fprint_folds.rds"))
result_ov = read_rds(here::here("data", "all_fprint_res_ov.rds"))
result_pct = read_rds(here::here("data", "all_fprint_res_testpct.rds"))
result %>%
  pull(name) %>%
  unique()

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


result_folds =
  result_folds %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random"),
         type = case_when(
           sub(".*\\d", "", name) == "xgb" ~ "XGBoost",
           sub(".*\\d", "", name) == "long" ~ "Long",
           sub(".*\\d", "", name) == "lasso" ~ "Lasso",
           sub(".*\\d", "", name) == "rf" ~ "Random Forest",
           sub(".*\\d", "", name) == "fnl" ~ "Linear SoFR",
           sub(".*\\d", "", name) == "nlfnl" ~ "Nonlinear SoFR",
           .default = "Logistic"))

result %>%
  select(n_sub, rank1_median, rank5_median, type, temporal) %>%
  pivot_longer(cols = contains("rank")) %>%
  mutate(name = factor(name, labels = c("Rank 1", "Rank 5"))) %>%
  ggplot(aes(x = n_sub, y = value, color = type)) +
  geom_jitter(width = 25)  +
  geom_line() +
  facet_grid(name~temporal) +
  scale_x_continuous(breaks=c(100, 500, 1000, 2500, 5000, 10000, 11125, 13367)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  theme(axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank()) +
  labs(x = "Number Subjects", y = "Accuracy") +
  scale_color_brewer(palette = "Dark2", name = "Model")


result %>%
  select(n_sub, rank1pct_median, rank5pct_median, type, temporal) %>%
  pivot_longer(cols = contains("rank")) %>%
  mutate(name = factor(name, labels = c("Rank 1", "Rank 5"))) %>%
  ggplot(aes(x = n_sub, y = value, color = type)) +
  geom_jitter(width = 25)  +
  geom_line() +
  facet_grid(name~temporal) +
  scale_x_continuous(breaks=c(100, 500, 1000, 2500, 5000, 10000, 11125, 13367)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  theme(axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank()) +
  labs(x = "Number Subjects", y = "Accuracy") +
  scale_color_brewer(palette = "Dark2", name = "Model")


result_ov %>%
  filter(factor < 1) %>%
  mutate(temporal = if_else(grepl("temporal", name), "Temporal", "Random")) %>%
  pivot_longer(cols = c(rank1_mean, rank5_mean), names_to = "metric", values_to = "value") %>%
  mutate(ovsamp = factor(factor)) %>%
  ggplot(aes(x = n_sub, y = value, color = ovsamp)) +
  geom_point() +
  geom_line(aes(group = ovsamp)) +
  facet_grid(temporal~metric, scales = "free_y") +
  theme_light()

# seems like 0.25% oversampling is best

result_pct %>%
  mutate(test_pct = n_test / n_train,
         n_sub = n_train) %>%
  pivot_longer(cols = c(rank1_mean, rank5_mean), names_to = "metric", values_to = "value") %>%
  mutate(test_pct = factor(test_pct)) %>%
  ggplot(aes(x = n_sub, y = value, color = test_pct)) +
  geom_point() +
  geom_line() +
  facet_grid(~metric)


result_folds %>%
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

lab_df =
  result_folds %>%
  filter(n == 100) %>%
  filter(type != "Long") %>%
  group_by(temporal, type) %>%
  summarize(med = median(rank1)) %>%
  ungroup()

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
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))

