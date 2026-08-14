library(tidyverse)

bench = tibble(test = seq(1, 6, 1),
               test_name = c("100", "500", "1000", "2500", "5000", "N"),
               elapsed = c(33.774, 107.229, 202.016, 492.843, 1024.732, 3138.554),
               relative = c(1.000, 3.175, 5.981, 14.592, 30.341, 92.928),
               user.self = c(29.969, 104.481, 198.813, 485.703, 997.698, 3034.587),
               sys.self = c(3.501, 1.831, 1.534, 3.077, 19.082, 78.754))


subsets100 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_100_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 100)
subsets500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_500_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 500)
subsets1000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_1000_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 1000)
subsets2500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_2500_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 2500)

subsets5000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_5000_v3.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 5000)

all = read_rds(here::here("results", "prediction_res_13367wtd.rds")) %>%
  mutate(across(-n_target, ~.x / n_target * 100)) %>%
  mutate(train_size = 13367)

comb =
  subsets100 %>%
  bind_rows(subsets500, subsets1000, subsets2500, subsets5000, all) %>%
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
  rename(train_size = test_name) %>%
  mutate(train_size = if_else(train_size == "N", "13367", train_size),
         train_size = as.numeric(train_size))

theme_set(theme_light(base_size = 14))

p1 = bench %>%
  ggplot(aes(x = train_size, y = elapsed / 50)) +
  geom_point() +
  geom_line() +
  labs(x = "Participants in training data (x100)", y = "Avg time to fit one model (sec)",
       title = "Runtime by number of participants in training data") +
  scale_x_continuous(labels = c(1, 5, 10, 25, 50, 133),
                     breaks = c(100, 500, 1000, 2500, 5000, 13367)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  theme(panel.grid.minor.x = element_blank())
# axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),


p2 = comb %>%
  select(rank1, rank5, train_size) %>%
  pivot_longer(cols = c(rank1, rank5)) %>%
  ggplot(aes(x = train_size, y = value, color = name)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(labels = c(1, 5, 10, 25, 50, 133),
                     breaks = c(100, 500, 1000, 2500, 5000, 13367)) +
  theme(panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  labs(x = "Participants in training data (x100)", y = "Accuracy (%)",
       title = "Accuracy by number of participants in training data") +
  scale_color_manual(values = c("#FF7F00FF", "#654CFFFF"),
                                labels = c("Rank 1", "Rank 5"),
                                name = "Metric") +
  theme(legend.position = c(0.8, 0.1))


library(patchwork)

p2 / p1 + plot_layout(axes = "collect")
