# scp lkoffman@jhpce-transfer01.jhsph.edu:/dcs05/ciprian/smart/nhanes_80hz/data/lily/data/all_fprint_res.rds data/
# scp lkoffman@jhpce-transfer01.jhsph.edu:/dcs05/ciprian/smart/nhanes_80hz/data/lily/data/all_fprint_res_ov.rds data/

library(tidyverse)
library(paletteer)
library(viridis)
x = read_rds(here::here("data", "all_fprint_res.rds"))
y = read_rds(here::here("data", "all_fprint_res_ov.rds"))
unique(x$name)

x %>%
  mutate(type = sub(".*\\d+", "", name),
         temporal = if_else(grepl("temporal", name), "Temporal", "Non-temporal"),
         type = if_else(type == "", "logistic", type)) %>%
  ggplot(aes(x = n_sub, y = rank1_mean, color = type)) +
  geom_jitter(width = 100) +
  # geom_line() +
  facet_grid(.~temporal)
  # select(name, temporal, type)
x %>%
  mutate(type = sub(".*\\d+", "", name),
         temporal = if_else(grepl("temporal", name), "Temporal", "Non-temporal"),
         type = if_else(type == "", "logistic", type)) %>%
  ggplot(aes(x = n_sub, y = rank5_mean, color = type)) +
  geom_jitter(width = 100) +
  # geom_line() +
  facet_grid(.~temporal)

x_reg =
  x %>%
  mutate(type = sub(".*\\d+", "", name),
         temporal = if_else(grepl("temporal", name), "Temporal", "Non-temporal"),
         type = if_else(type == "", "logistic", type)) %>%
  filter(type == "logistic" & temporal == "Non-temporal") %>%
  mutate(factor = 1 / n_sub)

x_reg %>%
  mutate(factor = factor(round(factor, 4))) %>%
  ggplot(aes(x = n_sub, y = rank1_mean, color = factor)) +
  geom_point() +
  # geom_line(aes(group = factor), linewidth = 1) +
  scale_color_paletteer_d("ggthemr::flat") +
# scale_color_viridis_d(option = "B", name = "% of data for predicted subject")+
  theme_light() +
  theme(legend.position = "bottom")

y %>%
  # mutate(factor = as.character(factor)) %>%
  # bind_rows(x_reg) %>%
  filter(n_sub <= 1000) %>%
  mutate(factor = factor(factor)) %>%
  ggplot(aes(x = n_sub, y = rank1_mean, color = factor)) +
  geom_point() +
  geom_line(aes(group = factor), linewidth = 1) +
  scale_color_paletteer_d("ggthemr::flat") +
  # scale_color_viridis_d(option = "B", name = "% of data for predicted subject")+
  theme_light() +
  theme(legend.position = "bottom")

  y %>%
    bind_rows(x_reg) %>%
    filter(n_sub <= 1000) %>%
    mutate(factor = factor(factor)) %>%
    ggplot(aes(x = n_sub, y = rank5_mean, color = factor)) +
    geom_point() +
    geom_line(aes(group = factor), linewidth = 1) +
    scale_color_paletteer_d("ggthemr::flat")
  # scale_color_viridis_d(option = "B", name = "% of data for predicted subject")+
  theme_light() +
    theme(legend.position = "bottom")
