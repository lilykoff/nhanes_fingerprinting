### generate figs
library(tidyverse)
options(digits.secs = 3)
library(tidymodels)
library(viridis)
library(gt)
library(gtsummary)
library(paletteer)
theme_set(theme_light())
walking_seg = readr::read_csv(here::here("data", "walking_segments.csv.gz"))
covars = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))
cells = readr::read_csv(here::here("data", "sample_grid_cells.csv.gz"))
col1 = "#6388B4FF"; col2 = "#FFAE34FF"; col3 = "#EF6F6AFF"; col4 = "#8CC2CAFF"
paletteer_d("colorBlindness::PairedColor12Steps")

col1 = "#FF7F00FF"; col2 = "#19B2FFFF"; col3 = "#654CFFFF"
paletteer_d("ggthemes::colorblind")
col1 = "#E69F00FF"; col2 = "#0072B2FF"; col3 = "#CC79A7FF"
raw_accel = read_csv(here::here("data", "67940.csv.gz"),
                     n_max = (60*80*60 + (80*10*60)))


p =
  raw_accel %>%
  rename(time = HEADER_TIMESTAMP) %>%
  mutate(min = as.numeric(difftime(time, min(time), units = "mins"))) %>%
  filter(min >= 20) %>%
  slice(1:(80*60)) %>%
  pivot_longer(cols = X:Z) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  labs(x = "Time (seconds)", y = bquote(Acceleration~(g~","~1*g == 9.81~m/s^2))) +
  scale_color_manual(values = c(col1, col2, col3),
                     labels = c("X", "Y", "Z"), name = "Axis") +
  theme_classic() +
  theme(legend.position = c(0.1, 0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_labels = "%S", date_breaks = "10 secs") +
  scale_y_continuous(breaks=seq(-2, 1, 0.5))


png(here::here("presentations", "figs", "raw_accel.png"), width = 6, height = 4, units = "in",
    res = 500)
print(p)
dev.off()

library(adept)
library(adeptdata)

temp = stride_template$left_wrist[[1]]


templates =
  stride_template$left_wrist[[5]] %>%
  as.data.frame() %>%
  mutate(type = paste0("Template ", row_number())) %>%
  pivot_longer(cols = -type) %>%
  group_by(type) %>%
  mutate(ind = row_number())


p = templates %>%
  mutate(type = factor(type)) %>%
  ggplot(aes(x = ind / 100, y = value, color = type)) +
  geom_line(linewidth = 1.1) +
  facet_grid(.~type) +
  theme_classic() +
  labs(y = "Acceleration (g)", x = "Time (seconds)") +
  scale_color_paletteer_d("colorBlindness::paletteMartin") +
  theme(legend.position = "none")

p = templates %>%
  mutate(type = factor(type)) %>%
  ggplot(aes(x = ind / 100, y = value, color = type)) +
  geom_line(linewidth = 1.1) +
  facet_grid(.~type) +
  theme_classic() +
  labs(y = "Acceleration (g)", x = "Time (seconds)") +
  scale_color_paletteer_d("ggthemes::colorblind", direction = -1) +
  theme(legend.position = "none")

png(here::here("presentations", "figs", "templates.png"), width = 7, height = 2, units = "in",
    res = 500)
print(p)
dev.off()


raw = read_rds(here::here("data", "raw_sample.rds"))
not_walking =
  raw %>%
  filter(minute(HEADER_TIMESTAMP) == 16 & second(HEADER_TIMESTAMP) >= 30 & second(HEADER_TIMESTAMP) <= 40)
not_walking2 =
  raw %>%
  filter(minute(HEADER_TIMESTAMP) == 16 & second(HEADER_TIMESTAMP) >= 40 & second(HEADER_TIMESTAMP) <= 50)

walking =
  raw %>%
  filter(walk) %>%
  slice(1:(10*80))

df =
  not_walking %>%
  bind_rows(walking) %>%
  bind_rows(not_walking2) %>%
  mutate(ind = row_number(),
         time2 = min(HEADER_TIMESTAMP) + as.period(ind / 80, "seconds"))

start = min(df %>% filter(walk) %>% pull(time2))
end = max(df %>% filter(walk) %>% pull(time2))
p = df %>%
  ggplot(aes(x = time2, y = vm, group = 1)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "#F0E442FF", alpha = 0.1,
            color = NA)  +
  geom_line() +
  scale_x_datetime(date_labels = "%S", date_breaks = "5 secs") +
  labs(x = "Time (seconds)", y = "Acceleration (g)") +
  theme_classic()

png(here::here("presentations", "figs", "walking_id.png"), width = 7, height = 2, units = "in",
    res = 500)
print(p)
dev.off()

walking =
  raw %>%
  filter(walk) %>%
  rename(time = HEADER_TIMESTAMP)

p1 = walking %>%
  slice(1:(80*5)) %>%
  mutate(s = second(floor_date(time, unit = "seconds"))) %>%
  ggplot(aes(x = time, y = vm, color = factor(s), group = 1)) +
  geom_line(linewidth = 1.1) +
  theme_classic() +
  theme(legend.position = "none" ) +
        # axis.text = element_text(size = 14),
        # axis.title = element_text(size = 16)) +
  # scale_color_viridis_d(option = "C") +
  scale_color_paletteer_d("ggthemes::colorblind") +
  geom_vline(aes(xintercept = min(time)), linetype = "dashed") +
  geom_vline(aes(xintercept = min(time) + as.period(1, "second")), linetype = "dashed") +
  geom_vline(aes(xintercept = min(time) + as.period(2, "second")), linetype = "dashed") +
  geom_vline(aes(xintercept = min(time) + as.period(3, "second")), linetype = "dashed") +
  geom_vline(aes(xintercept = min(time) + as.period(4, "second")), linetype = "dashed") +
  geom_vline(aes(xintercept = min(time) + as.period(5, "second")), linetype = "dashed") +
  labs(x = "Time (seconds)", y = "Acceleration (g)") +
  scale_x_datetime(date_labels = "%S", date_breaks = "1 secs")


labels_df = walking %>%
  # slice(1:(80 * 3)) %>%
  # mutate(s = second(floor_date(time, unit = "seconds")),
  #        s = factor(s, labels = c("Second 1", "Second 2", "Second 3"))) %>%
  slice(1:(80 * 2)) %>%
  mutate(s = second(floor_date(time, unit = "seconds")),
         s = factor(s, labels = c("Second 1", "Second 2"))) %>%
  group_by(s) %>%
  mutate(vm_lag = lag(vm, n = 12L)) %>%
  filter(!is.na(vm_lag)) %>%
  group_by(s) %>%
  slice(c(10, 40)) %>%
  pivot_longer(cols = contains("vm")) %>%
  # group_by(s, name) %>%
  # slice(14) %>% # Select two points per group to label
  mutate(label = if_else(name == "vm", paste0("g=", round(value, 2)), paste0("lag g=", round(value, 2))))

library(ggrepel)
p2 = walking %>%
  # slice(1:(80*3)) %>%
  slice(1:(80*2)) %>%
  mutate(s = second(floor_date(time, unit = "seconds")),
         s = factor(s, labels = c("Second 1", "Second 2"))) %>%
         # s = factor(s, labels = c("Second 1", "Second 2", "Second 3"))) %>%
  group_by(s) %>%
  mutate(vm_lag = lag(vm, n = 12L)) %>%
  pivot_longer(cols = contains("vm")) %>%
  ggplot(aes(x = time, y = value, color = name, linetype = name)) +
  geom_line() +
  geom_point(size = .4) +
  facet_grid(.~s, scales = "free_x") +
  # theme(axis.text.x = element_blank()) +
  labs(x = "Time (seconds)", y = "Acceleration (g)") +
  theme_classic() +
  theme(legend.position = "none") +
  #       axis.text = element_text(size = 14),
  #       axis.title = element_text(size = 16),
  #       strip.text = element_text(size = 14)) +
  geom_vline(data = labels_df, aes(xintercept = time),
             linetype = "dashed") +
  geom_label_repel(data = labels_df, aes(x = time, y = value, label = label),
                   size = 2, inherit.aes = FALSE,
                   box.padding = 0, min.segment.length = 0,
                   segment.color = "black", segment.size = 0.5) +
  scale_color_manual(values = c("#009E73FF", "#56B4E9FF"))

paletteer_c("grDevices::Viridis", 5)

gridlines =
  tibble(vm = seq(0.5, 2, 0.25),
         vm_lag = seq(0.5, 2, 0.25))
p3 = walking %>%
  # slice(1:(80*3)) %>%
  # mutate(s = second(floor_date(time, unit = "seconds")),
  #        s = factor(s, labels = c("Second 1", "Second 2", "Second 3"))) %>%
  slice(1:(80 * 2)) %>%
  mutate(s = second(floor_date(time, unit = "seconds")),
         s = factor(s, labels = c("Second 1", "Second 2"))) %>%
  group_by(s) %>%
  mutate(vm_lag = lag(vm, n = 12L)) %>%
  # pivot_longer(cols = contains("vm")) %>%
  ggplot(aes(x = vm, y = vm_lag, color = s)) +
  geom_point() +
  facet_grid(.~s) +
  # scale_color_manual(values = c("#4B0055FF", "#00588BFF", "#009B95FF")) +
  scale_color_manual(values = c("#D55E00FF", "#0072B2FF", "#CC79A7FF")) +
  theme(legend.position = "none") +
        # axis.text = element_text(size = 14),
        # axis.title = element_text(size = 16),
        # strip.text = element_text(size = 14)) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  geom_vline(data = gridlines, aes(xintercept = vm), linewidth = .25, alpha = .6) +
  geom_hline(data = gridlines, aes(yintercept = vm), linewidth = .25, alpha = .6)

library(patchwork)

p_spacer <- wrap_elements(grid::nullGrob())

p = (p1 + p2) / (p3 + p_spacer) +  plot_annotation(tag_levels = 'I')


png(here::here("presentations", "figs", "process.png"), width = 14.7, height = 12.7, units = "cm",
    res = 500)
print(p)
dev.off()


result_all = read_rds(here::here("data", "all_fprint_folds.rds"))
result = read_rds(here::here("data", "all_fprint_res.rds"))
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

p =
  result_all %>%
    filter(type != "Long") %>%
  mutate(type = fct_reorder(type, rank1, median, .desc = TRUE)) %>%
  ggplot(aes(x = type, y = rank1, fill = temporal)) +
  geom_boxplot(outlier.colour=NA, position=position_dodge(width=0.8)) +
  geom_point(position=position_jitterdodge(dodge.width=0.8, jitter.width = .2), alpha= .4, size = .2) +
  labs(x= "", y = "Rank 1 Accuracy", title = "Rank 1 accuracy in subsets of 100 subjects") +
  scale_fill_manual(values =c("#0072B2FF", "#CC79A7FF"), name = "Train/Test Split") +
  theme(legend.position = c(.8, .8),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(0,100, 15), limits = c(0,100)) +
  stat_summary(
    fun = median,
    geom = "label",
    aes(label = round(after_stat(y), 1)),
    position = position_dodge(width = 0.8),
    vjust = -2.5,
    size = 3,
    show.legend = FALSE
  )

png(here::here("presentations", "figs", "acc100.png"), width = 7, height = 5, units = "in",
    res = 500)
print(p)
dev.off()


p = result %>%
  select(n_sub, rank1_median, rank5_median, rank1pct_median, rank5pct_median, type, temporal) %>%
  filter(type == "Logistic" & n_sub %in% c(100, 500, 1000, 2500, 5000, 10000, 11225, 13367)) %>%
  pivot_longer(cols = contains("rank")) %>%
  mutate(name = factor(name, levels = c("rank1_median", "rank5_median", "rank1pct_median", "rank5pct_median"),
                       labels = c("Rank 1", "Rank 5", "Rank 1%", "Rank 5%"))) %>%
  ggplot(aes(x = n_sub, y = value, color = name)) +
  geom_point()  +
  geom_line(linewidth = 1) +
  facet_grid(.~temporal, scales = "free_x") +
  scale_x_continuous(breaks=c(100, 500, 1000, 2500, 5000, 10000, 11125, 13367),
                     labels = c(1, 5, 10, 25, 50, 100, 111, 133)) +
  scale_y_continuous(breaks=seq(0,100,15), limits = c(0,100)) +
  labs(x = "Number Subjects (x100)", y = "Accuracy", title = "Logistic regression models: accuracy with varying sample size") +
  scale_color_paletteer_d("ggthemes::colorblind", name = "Metric") +
  theme(legend.position = c(0.7, 0.8),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12)) +
  guides(color = guide_legend(nrow = 1))

png(here::here("presentations", "figs", "acc_n.png"), width = 7, height = 5, units = "in",
    res = 500)
print(p)
dev.off()

### code to get walking seg cluster
library(tidyverse)

files = list.files(here::here("data", "lily", "data", "adept_walking_dfs", "pax_h"), recursive = TRUE,
                   full.names = TRUE)


x = read_csv(files[1001])
files[1001]

raw = read_csv(here::here("data", "csv", "pax_h","74867.csv.gz"))

segments_10 = x %>%
  mutate(day = floor_date(second, unit = "days")) %>%
  select(second, day) %>%
  distinct() %>%
  mutate(timediff = as.numeric(difftime(second, dplyr::lag(second, n = 1), units = "secs")),
         ltwosec = (timediff <= 1)*1,
         rleid = data.table::rleid(ltwosec)) %>%
  filter(ltwosec == 1) %>%
  group_by(rleid, day) %>%
  summarize(n_seconds = n(),
            start = min(second),
            end = max(second)) %>%
  filter(n_seconds >= 10)

# key of those times
seconds_key =
  segments_10 %>%
  ungroup() %>%
  slice(1) %>%
  group_by(rleid, day) %>%
  tidyr::expand(second = seq(start, end, "sec"))


# raw = read_csv(here::here("data", "csv", "pax_h","74867.csv.gz"))

# want from 2000-01-08 17:35 to 2000-01-08 17:40

small =
  raw %>%
  filter(HEADER_TIMESTAMP >= as.POSIXct("2000-01-09 14:10:00", tz = "UTC") &
           HEADER_TIMESTAMP <= as.POSIXct("2000-01-09 14:20:00", tz = "UTC")) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2))

small =
  small %>%
  mutate(second = floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
  left_join(seconds_key, by = "second") %>%
  mutate(walk = if_else(is.na(rleid), FALSE, TRUE))


write_rds(small, here::here("data", "lily", "data", "raw_sample.rds"),
          compress ="xz")

