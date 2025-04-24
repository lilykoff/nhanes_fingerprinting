### generate figs
library(tidyverse)
options(digits.secs = 3)
library(tidymodels)
library(viridis)
library(gt)
library(gtsummary)
library(paletteer)
library(patchwork)
theme_set(theme_light(base_size = 14))
raw_accel = read_csv(here::here("data", "67940.csv.gz"),
                     n_max = (60*80*60 + (80*10*60))) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2))

p1 = raw_accel %>%
  slice(1:(60*80)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_line() +
  scale_x_datetime(date_labels = "%S", date_breaks = "10 sec")  +
  labs(x = "Time (s)", y = "Acceleration (g)")


p2 = raw_accel %>%
  slice(1:(60*80)) %>%
  pivot_longer(cols = X:Z) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = value, color = name)) +
  scale_x_datetime(date_labels = "%S", date_breaks = "10 sec")  +
  geom_line() +
  labs(x = "Time (s)", y = "Acceleration (g)") +
  scale_color_paletteer_d("ggthemes::colorblind", direction = -1, name = "Axis") +
  theme(legend.position = c(0.2, 0.2)) +
  guides(color = guide_legend(nrow = 1))

plt = p2 / p1 + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = "A")

png(here::here("manuscript", "figs", "raw_accel.png"), width = 8, height = 6, units = "in",
    res = 500)
print(plt)
dev.off()


data2 =
  tibble(day = c(1, 2, 3, 4),
         start = c(0, NA, NA, 20),
         end = c(135,NA, NA, 65),
         type = c("train", "train", "train", "test"))

df =
  expand_grid(day = 1:4,
              minute = 1:180) %>%
  mutate(rn = row_number())

all_inds = sample(1:720, 180, replace = FALSE)
train_inds = sample(all_inds, 180*.75, replace = FALSE)
test_inds = setdiff(all_inds, train_inds)

df =
  df %>%
  mutate(type = case_when(
    rn %in% train_inds ~ "train",
    rn %in% test_inds ~ "test",
    .default = NA_character_
  ),
  start = if_else(!is.na(type), minute, NA_real_),
  end = start + 1)

# df %>%
#   ggplot() +
#   geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 1,
#                 fill = type)) +
#   facet_grid(.~day) +
#   scale_y_continuous(limits = c(0, 1), breaks = NULL) +
#   scale_x_continuous(breaks=seq(0,180,30),
#                      labels =seq(0,3,0.5),
#                      limits=c(0,180))
p = df %>%
  mutate(paradigm = "random") %>%
  bind_rows(data2 %>% mutate(paradigm = "temporal")) %>%
  mutate(
    type = factor(type, levels = c("train", "test")),
    y = as.numeric(factor(paradigm, levels = c("temporal", "random"))),
    ymin = y - 0.3,
    ymax = y + 0.3,
    day = paste0("Day ", day)
  ) %>%
  ggplot(aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax, fill = type)) +
  geom_rect() +
  scale_y_continuous(breaks = 1:2, expand = c(0.05, 0.05),
                     labels = c("Temporal", "Random")) +
  facet_grid(~day) +
  # scale_y_continuous(limits = c(0, 1), breaks = NULL) +
  scale_x_continuous(breaks=seq(0,180,60),
                     labels =seq(0,3,1),
                     limits=c(0,180)) +
  labs(x = "Minutes") +
  scale_fill_manual(values = c("#006DDBFF", "#DB6D00FF"), na.translate = FALSE, name = "") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank())

png(here::here("manuscript", "figs", "train_tet.png"), width = 6, height = 4, units = "in",
    res = 500)
p
dev.off()
