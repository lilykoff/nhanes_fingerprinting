library(tidyverse)
library(patchwork)

bench = read_rds(here::here("results", "bench_adept.rds"))


subsets100 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_100_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 100)

# subsets100_v2 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_100_v4.rds")) %>%
#   mutate(across(-n_target, ~.x / n_target * 100)) %>%
#   mutate(train_size = 100)
subsets500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_500_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 500)
subsets1000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_1000_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 1000)
subsets2500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_2500_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 2500)


subsets2500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_2500_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 2500)

subsets5000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_5000_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 5000)

subsets5000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_5000_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 5000)

subsets7500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_7500_v4.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 7500)

all = read_rds(here::here("results", "prediction_res_13367wtd.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 13367)

comb =
  subsets100 %>%
  bind_rows(subsets500, subsets1000, subsets2500, subsets5000, subsets7500, all) %>%
  select(-fold)

comb %>%
  pivot_longer(cols = contains("rank")) %>%
  mutate(type = if_else(grepl("pct", name), "Percent", "Absolute"),
         rank = if_else(grepl("1", name), "Rank 1", "Rank 5")) %>%
  ggplot(aes(x = train_size, y = value, color = rank)) +
  facet_wrap(.~type) +
  geom_point()

bench =
  bench %>%
  rename(train_size = test) %>%
  mutate(train_size = if_else(train_size == "N", "13367", train_size),
         train_size = as.numeric(train_size))

theme_set(theme_light(base_size = 14))

p1 = bench %>%
  ggplot(aes(x = train_size, y = elapsed / replications)) +
  geom_point() +
  geom_line() +
  labs(x = "Participants in training data (x100)", y = "Avg time to fit one model (sec)",
       title = "Runtime by number of participants in training data") +
  scale_x_continuous(labels = c(1, 5, 10, 25, 50, 75, 100, 133),
                     breaks = c(100, 500, 1000, 2500, 5000, 7500, 10000, 13367)) +
  # scale_y_continuous(breaks = seq(0, 70, 10)) +
  theme(panel.grid.minor.x = element_blank())
# axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),


p2 = comb %>%
  select(rank1, rank5, train_size) %>%
  pivot_longer(cols = c(rank1, rank5)) %>%
  ggplot(aes(x = train_size, y = value, color = name)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(labels = c(1, 5, 10, 25, 50, 75, 100, 133),
                     breaks = c(100, 500, 1000, 2500, 5000, 7500, 10000, 13367)) +
  theme(panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  labs(x = "Participants in training data (x100)", y = "Accuracy (%)",
       title = "Accuracy by number of participants in training data") +
  scale_color_manual(values = c("#FF7F00FF", "#654CFFFF"),
                                labels = c("Rank 1", "Rank 5"),
                                name = "Metric") +
  theme(legend.position = c(0.8, 0.1))



p2 / p1 + plot_layout(axes = "collect")

png(here::here("manuscript", "figs_final", "subsets.png"), width = 8, height = 6, res = 350, units = "in")
p2 / p1 + plot_layout(axes = "collect") + plot_annotation(tag_levels = "A")
dev.off()
