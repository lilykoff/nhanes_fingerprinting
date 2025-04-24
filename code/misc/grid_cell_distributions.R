library(tidyverse)
library(tidymodels)
library(viridis)
# df = readr::read_csv(here::here("data", "lily", "data", "sample_grid_cells.csv.gz"))
df = readr::read_csv(here::here("data", "all_grid_cells.csv.gz"))
# everyone has 180 sec of data
x_vars = seq(0, 3, 0.25)

df2 = tibble(vm = seq(0, 3, 0.125)) %>%
  mutate(lag_vm = dplyr::lag(vm, n = 1)) %>%   # for each second, calculate vm and lagged vm
  mutate(
    cut_sig = cut(
      vm,
      breaks = seq(0, 3, by = 0.25),
      include.lowest = T
    ),
    cut_lagsig = cut(
      lag_vm,
      breaks = seq(0, 3, by = 0.25),
      include.lowest = T
    )
  ) %>%
  drop_na() %>% # count # points in each "grid cell"
  count(cut_sig, cut_lagsig, .drop = FALSE) %>%
  mutate(
    cell = paste(cut_sig, cut_lagsig, sep = "_"),
    num_x  = as.numeric(cut_sig),
    num_y = as.numeric(cut_lagsig)
  )

old_names = unique(df2$cell)
# clean_names = janitor::clean_names(old_names)

temp =
  tibble(x = old_names,
         y = seq(1:length(old_names))) %>%
  pivot_wider(names_from = x, values_from = y)
clean_names = janitor::clean_names(temp) %>%
  colnames()

key = tibble(old_names, clean_names)


df2 = df2 %>%
  full_join(key, by = c("cell" = "old_names"))

df_summ =
  df %>%
  group_by(id) %>%
  mutate(n = n()) %>%
  group_by(id, n) %>%
  summarize(across(starts_with("x"), ~sum(.x))) %>%
  ungroup() %>%
  mutate(across(ends_with("12"), ~.x / (n * (80-12))),
         across(ends_with("24"), ~.x / (n * (80-24))),
         across(ends_with("36"), ~.x / (n * (80-36))))

df_means =
  df_summ %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("x")) %>%
  group_by(name) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            max = max(value),
            min = min(value)) %>%
  mutate(lag = sub(".*_(.*)", "\\1", name),
         name = sub("(.*)_.*", "\\1", name)) %>%
  left_join(df2, by = c("name" = "clean_names"))

df_means %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = mean)) +
  geom_tile(col = "black") +
  scale_fill_viridis() +
  facet_wrap(.~lag) +
  geom_text(data = df_means %>% filter(mean > 0.01), aes(x = cut_sig, y = cut_lagsig, label = round(mean, 2)), size = 2)

df_means %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = sd)) +
  geom_tile(col = "black") +
  scale_fill_viridis() +
  facet_wrap(.~lag) +
  geom_text(data = df_means %>% filter(sd > 0.01), aes(x = cut_sig, y = cut_lagsig, label = round(sd, 2)), size = 2)

df_means %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = max)) +
  geom_tile(col = "black") +
  scale_fill_viridis() +
  facet_wrap(.~lag) +
  geom_text(data = df_means %>% filter(max > 0.01), aes(x = cut_sig, y = cut_lagsig, label = round(max, 2)), size = 2)



covars = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

df_means2 =
  df_summ %>%
  mutate(id = as.character(id)) %>%
  left_join(covars %>% select(id = SEQN, gender, age = age_in_years_at_screening, mortstat))

df_means2 %>%
  filter(!is.na(gender)) %>%
  pivot_longer(cols = starts_with("x")) %>%
  group_by(name, gender) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            max = max(value),
            min = min(value)) %>%
  mutate(lag = sub(".*_(.*)", "\\1", name),
         name = sub("(.*)_.*", "\\1", name)) %>%
  left_join(df2, by = c("name" = "clean_names")) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = max)) +
  geom_tile(col = "black") +
  scale_fill_viridis() +
  facet_grid(gender~lag) +
  geom_text(data = df_means %>% filter(max > 0.01), aes(x = cut_sig, y = cut_lagsig, label = round(max, 2)), size = 2)

df_means2 %>%
  filter(!is.na(gender)) %>%
  pivot_longer(cols = starts_with("x")) %>%
  group_by(name, gender) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            max = max(value),
            min = min(value)) %>%
  mutate(lag = sub(".*_(.*)", "\\1", name),
         name = sub("(.*)_.*", "\\1", name)) %>%
  left_join(df2, by = c("name" = "clean_names")) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = mean)) +
  geom_tile(col = "black") +
  scale_fill_viridis() +
  facet_grid(gender~lag) +
  geom_text(data = df_means %>% filter(mean > 0.01), aes(x = cut_sig, y = cut_lagsig, label = round(mean, 2)), size = 2)

df_means2 %>%
  filter(age >= 50 & !is.na(mortstat)) %>%
  pivot_longer(cols = starts_with("x")) %>%
  group_by(name, mortstat) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            median = median(value),
            max = max(value),
            min = min(value)) %>%
  mutate(lag = sub(".*_(.*)", "\\1", name),
         name = sub("(.*)_.*", "\\1", name)) %>%
  left_join(df2, by = c("name" = "clean_names")) %>%
  ggplot(aes(x =cut_sig, y = cut_lagsig, fill = mean)) +
  geom_tile(col = "black") +
  scale_fill_viridis() +
  facet_grid(mortstat~lag) +
  geom_text(data = df_means %>% filter(mean > 0.01), aes(x = cut_sig, y = cut_lagsig, label = round(mean, 2)), size = 2)



