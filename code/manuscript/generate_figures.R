library(tidyverse)
library(glue)
library(paletteer)
library(patchwork)
theme_set(theme_light(base_size = 14))

library(gt)


all_subs = read_rds(here::here("data", "fingerprint_folds.rds"))
temporal_subs = read_rds(here::here("data", "fingerprint_folds_temporal2.rds"))

### accuracies for n=100,n=500

all_100 = read_rds(here::here("results", "all_n100.rds"))

all_500 = read_rds(here::here("results", "all_n500.rds"))

p1 = all_100 %>%
  bind_rows(all_500 %>% mutate(fold = as.character(fold))) %>%
  mutate(across(c(rank1, rank5), ~(.x / n)*100)) %>%
  mutate(n = factor(n, labels = c("n = 100", "n = 500"))) %>%
  group_by(paradigm) %>%
  mutate(type = factor(type, levels = c("Logistic", "Lasso", "XGBoost", "Random Forest", "Nonlinear SoFR", "Linear SoFR")),
         paradigm = factor(paradigm, levels = c("random", "temporal2"), labels = c("Random", "Temporal"))) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, labels = c("Rank 1", "Rank 5"))) %>%
  filter(paradigm == "Random") %>%
  ggplot(aes(x = type, y = value, color = type)) +
  geom_boxplot() +
  geom_jitter(width = .25, alpha = .5, size = .4) +
  scale_color_manual(values = c("#000000FF", "#009292FF", "#DB6D00FF", "#006DDBFF", "#B66DFFFF", "#920000FF"), name = "Model") +
  # geom_point(position = position_jitterdodge(), alpha = .2, size = .2) +
  facet_grid(metric~n) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 30, vjust = .8),
        # legend.position = c(0.75, 0.45),
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Accuracy", title = "Random paradigm") +
  # scale_x_discrete(labels = c("Rank 1", "Rank 5")) +
  guides(color = guide_legend(nrow = 2)) +
  scale_x_discrete(labels =c("Logistic", "Lasso", "XGBoost", "RF", "nlSoFR", "lSoFR"))


p2 = all_100 %>%
  bind_rows(all_500 %>% mutate(fold = as.character(fold))) %>%
  mutate(across(c(rank1, rank5), ~(.x / n)*100)) %>%
  mutate(n = factor(n, labels = c("n = 100", "n = 500"))) %>%
  group_by(paradigm) %>%
  mutate(type = factor(type, levels = c("Logistic", "Lasso", "XGBoost", "Random Forest", "Nonlinear SoFR", "Linear SoFR")),
         paradigm = factor(paradigm, levels = c("random", "temporal2"), labels = c("Random", "Temporal"))) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, labels = c("Rank 1", "Rank 5"))) %>%
  filter(paradigm == "Temporal") %>%
  ggplot(aes(x = type, y = value, color = type)) +
  geom_boxplot() +
  geom_jitter(width = .25, alpha = .5, size = .4) +
  scale_color_manual(values = c("#000000FF", "#009292FF", "#DB6D00FF", "#006DDBFF", "#B66DFFFF", "#920000FF"), name = "Model") +
  # geom_point(position = position_jitterdodge(), alpha = .2, size = .2) +
  facet_grid(metric~n) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 30, vjust = .8),
        # legend.position = c(0.75, 0.45),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits=c(0,100)) +
  labs(x = "", y = "Accuracy", title = "Temporal paradigm") +
  # scale_x_discrete(labels = c("Rank 1", "Rank 5")) +
  guides(color = guide_legend(nrow = 2)) +
  scale_x_discrete(labels =c("Logistic", "Lasso", "XGBoost", "RF", "nlSoFR", "lSoFR"))

### here for RF
png(here::here("manuscript", "figs_final", "acc100_500.png"), width = 8, height = 7, # 5.3
    units = "in",
    res = 350)
# print(p)
p1 / p2  + plot_layout(axes = "collect")
# plot_annotation(tag_levels = "A")
dev.off()

all_logistic = read_rds(here::here("results", "all_logistic.rds"))

lab_df =
  all_logistic %>%
  mutate(paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  group_by(n, paradigm) %>%
  summarize(across(starts_with("rank"), median)) %>%
  pivot_longer(cols = contains("rank"), names_to = "metric", values_to = "value") %>%
  ungroup() %>%
  filter(n == 5000) %>%
  mutate(metric_lab =
           case_when(metric == "rank1" ~ "Rank 1",
                     metric == "rank5" ~ "Rank 5",
                     metric == "rank1pct" ~ "Rank 1%",
                     .default = "Rank 5%"))
p = all_logistic %>%
  mutate(paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  group_by(n, paradigm) %>%
  summarize(across(starts_with("rank"), median)) %>%
  pivot_longer(cols = contains("rank"), names_to = "metric", values_to = "value") %>%
  # mutate(name = factor(name, labels = c("Rank 1", "Rank 5"))) %>%
  ggplot(aes(x = n, y = value, color = metric, group = metric)) +
  geom_jitter(width = 25, size = 3) +
  geom_line(linewidth = 1.4) +
  facet_grid(.~paradigm, scales = "free_x") +
  scale_x_continuous(breaks=c(100, 500, 1000, 2500, 5000, 10000, 10770, 13367),
                     labels = c(1, 5, 10, 25, 50, 100, 108, 133)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  geom_label(data = lab_df,
             aes(x = n, y = value, label = metric_lab, color = metric),
             hjust = -.5, size = 4) +
  labs(x = "Number of Participants (x100)", y = "Median Accuracy") +
  scale_color_manual(values = c("#FF7F00FF", "#FFBF7FFF", "#654CFFFF", "#CCBFFFFF"),
                     labels = c("Rank 1", "Rank 1%", "Rank 5", "Rank 5%"), name = "Metric")


png(here::here("manuscript", "figs_final", "acc_pct.png"), width = 6, height = 4, res = 350, units = "in")
p
dev.off()

all_long = read_rds(here::here("results", "all_long.rds"))
p = all_long %>%
  mutate(long = "Long") %>%
  bind_rows(all_logistic %>% mutate(long = "Default")) %>%
  filter(n %in% c(100, 1000, 2500, 5000, 10129, 10000,8018)) %>%
  mutate(paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  group_by(paradigm, long, n) %>%
  summarize(across(c(rank1, rank5), median), .groups = "drop") %>%
  pivot_longer(cols = starts_with("rank"), names_to = "metric") %>%
  ggplot(aes(x = n, y = value, color = long, linetype = metric)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.1) +
  facet_wrap(.~paradigm, scale = "free_x") +
  scale_x_continuous(breaks=c(100,  1000, 2500, 5000, 10129),
                     labels = c(1, 10, 25, 50, 100)) +
  scale_y_continuous(breaks=seq(0,100, 10), limits = c(0,100)) +
  guides(color = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1)) +
  scale_linetype_discrete(name = "Metric", labels = c("Rank 1", "Rank 5")) +
  # scale_shape_discrete(name = "Metric", labels = c("Rank 1", "Rank 5")) +
  scale_color_manual(name = "Amount of Data", labels = c("3 minutes", "6 minutes"),
                     values = c("#924900FF", "#6DB6FFFF")) +
  labs(x = "Number of Subjects (x100)", y = "Median Accuracy") +
  theme(legend.position = c(.8, .7))

png(here::here("manuscript", "figs_final", "long_acc.png"), width = 8, height = 4, units = "in",
    res = 350)
p
dev.off()
## oversampling

small = c(here::here("results", "prediction_res_100over.rds"),
          here::here("results", "prediction_res_500over.rds"),
          here::here("results", "prediction_res_1000over.rds"),
          here::here("results", "prediction_res_temporal2_100over.rds"),
          here::here("results", "prediction_res_temporal2_500over.rds"),
          here::here("results", "prediction_res_temporal2_1000over.rds"))
res_over = map_dfr(small, .f = \(x) read_rds(x) %>% mutate(name = x)) %>%
  ungroup() %>%
  mutate(paradigm = if_else(grepl("temporal", name), "temporal", "random"),
         across(contains("rank"), ~.x / n)) %>%
  group_by(paradigm, n, factor) %>%
  summarize(across(contains("rank"), ~median(.x) * 100),
            .groups = "drop")

default = all_logistic %>%
  filter(type == "Logistic") %>%
  filter(n_tar %in% c(100, 500, 1000)) %>%
  group_by(n_tar, paradigm) %>%
  summarize(across(contains("rank"), median),
            .groups = "drop") %>%
  mutate(paradigm = if_else(paradigm == "temporal2", "temporal", "random")) %>%
  rename(n = n_tar) %>%
  mutate(factor = 1 / n,
         factor = "None")

p = res_over %>%
  filter(factor < 1) %>%
  mutate(factor = as.character(factor)) %>%
  bind_rows(default) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "metric", values_to = "value") %>%
  mutate(ovsamp = factor(factor),
         metric = factor(metric, labels = c("Rank 1", "Rank 5")),
         paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  ggplot(aes(x = n, y = value, color = ovsamp)) +
  geom_point(size = 2) +
  # geom_textline(aes(group = ovsamp, label = ovsamp), linewidth = .9)+
  geom_line(aes(group = ovsamp), linewidth = .9) +
  facet_grid(paradigm~metric, scales = "free_y") +
  theme_light() +
  scale_color_manual(values = c("#290AD8FF", "#3FA0FFFF", "#FFE099FF",
                                "#FFAD72FF", "#F76D5EFF", "grey"),
                     # name = "Proportion of data\ncomprised by outcome")+
                     name = "Oversampling\nfactor") +
  labs(x = "Subgroup Size", y = "Median Accuracy") +
  scale_x_continuous(breaks=c(100, 500, 1000), labels = c(100, 500, 1000))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))
png(
  here::here("manuscript", "figs_final", "oversamp_acc.png"),
  width = 6,
  height = 4,
  units = "in",
  res = 350
)
# print(p)
p
dev.off()




##### train test figure
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
  scale_fill_manual(values = c("#006DDBFF", "#DB6D00FF"), na.translate = FALSE, name = "", labels = c("Train", "Test")) +
  theme(legend.position = c(.4, .2)) +
  theme(panel.grid = element_blank())

png(here::here("manuscript", "figs", "train_test.png"), width = 6, height = 4, units = "in",
    res = 100)
p
dev.off()

### figure 1
raw_accel = read_csv(here::here("data", "67940.csv.gz"))
# n_max = (60*80*60 + (80*10*60)))


day1 = raw_accel %>%
  mutate(day = floor_date(HEADER_TIMESTAMP, unit = "days")) %>%
  filter(day == day[1])

day2 =
  raw_accel %>%
  mutate(day = floor_date(HEADER_TIMESTAMP, unit = "days")) %>%
  filter(day == as.Date("2000-01-03"))

rm(raw_accel)

paletteer::paletteer_d("colorBlindness::Blue2DarkRed12Steps")

p1 = day2 %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_rect(aes(xmin = as.POSIXct("2000-01-03 08:00:00.000", tz = "UTC"),
                xmax = as.POSIXct("2000-01-03 09:00:00.000", tz = "UTC"), ymin = -Inf, ymax = Inf),
            fill = "#FFAD72FF", alpha = 0.5) +
  geom_line(linewidth = .05) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H") +
  theme(panel.grid = element_blank()) +
  labs(x = "Time of day", y = "Acceleration (g)") +
  scale_y_continuous(limits=c(0,7))


step_result = read_rds(here::here("data", "plot_df_step_result.rds"))


# the steps per second
steps_seconds =
  day2 %>%
  mutate(row_ind = row_number()) %>%
  left_join(., step_result, by = c("row_ind" = "tau_i")) %>%
  mutate(
    steps = ifelse(is.na(steps), 0, steps),
    time = lubridate::floor_date(HEADER_TIMESTAMP, unit = "seconds")
  ) %>%
  group_by(time) %>%
  summarize(steps_adept = sum(steps)) %>%
  select(time, steps_adept) %>%
  filter(steps_adept > 0)

segments_10 = steps_seconds %>%
  rename(second = time) %>%
  select(second) %>%
  mutate(timediff = as.numeric(difftime(second, dplyr::lag(second, n = 1), units = "secs")),
         ltwosec = (timediff <= 2)*1,
         rleid = data.table::rleid(ltwosec)) %>%
  filter(ltwosec == 1) %>%
  group_by(rleid) %>%
  summarize(n_seconds = n(),
            start = min(second),
            end = max(second)) %>%
  filter(n_seconds >= 9)
## zoom in

start = as.POSIXct("2000-01-03 08:00:00", tz = "UTC")
second_df =
  steps_seconds %>%
  mutate(HEADER_TIMESTAMP = time) %>%
  filter(HEADER_TIMESTAMP >= start & HEADER_TIMESTAMP <= start + as.period(1, "hour")) %>%
  filter(HEADER_TIMESTAMP != as.POSIXct("2000-01-03 08:03:49", tz = "UTC"))
p2 = day2 %>%
  filter(HEADER_TIMESTAMP >= start & HEADER_TIMESTAMP <= start + as.period(1, "hour")) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_rect(data = second_df,
            aes(xmin = HEADER_TIMESTAMP,
                xmax = HEADER_TIMESTAMP + as.period(1, "second"),
                ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#3FA0FFFF") +
  geom_rect(aes(xmin = as.POSIXct("2000-01-03 08:03:36.000", tz = "UTC"),
                xmax = as.POSIXct("2000-01-03 08:04:01.000", tz = "UTC"), ymin = -Inf, ymax = Inf),
            fill = "#D82632FF", alpha = .5) +
  geom_line(linewidth = .1) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "10 min") +
  labs(x = "Time of day", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())

start = as.POSIXct("2000-01-03 08:03:36.000", tz = "UTC")
end = as.POSIXct("2000-01-03 08:04:01.000", tz = "UTC")
p3 = day2 %>%
  filter(between(HEADER_TIMESTAMP, start, end)) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_rect(data = second_df %>% filter(between(HEADER_TIMESTAMP, start, end - as.period(1, "second"))),
            aes(xmin = HEADER_TIMESTAMP,
                xmax = HEADER_TIMESTAMP + as.period(1, "second"),
                ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#D82632FF", alpha = 0.5) +
  geom_line(linewidth = .5) +
  scale_x_datetime(date_breaks = "5 sec", date_labels =  "%H:%M:%S") +
  labs(x = "Time of day", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())
png(here::here("manuscript", "figs_final", "figure1_3panel.png"), width = 6, height = 8, units = "in",
    res = 150)
p1 / p2 / p3 + plot_annotation(tag_levels = "A")
dev.off()

p1 = day2 %>%
  filter(between(HEADER_TIMESTAMP, start, end)) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_rect(data = second_df %>% filter(between(HEADER_TIMESTAMP, start, end - as.period(1, "second"))),
            aes(xmin = HEADER_TIMESTAMP,
                xmax = HEADER_TIMESTAMP + as.period(1, "second"),
                ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#D82632FF", alpha = 0.5) +
  geom_line(linewidth = .5) +
  scale_x_datetime(date_breaks = "5 sec", date_labels =  "%H:%M:%S") +
  labs(x = "Time of day", y = "Acceleration (g)") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  annotate(geom = "segment", x = as.POSIXct("2000-01-03 08:03:36", tz = "UTC"),
           xend = as.POSIXct("2000-01-03 08:03:48", tz = "UTC"),
           y = 2,
           color = "#290AD8FF", linewidth = 2) +
  annotate(geom = "label", x = as.POSIXct("2000-01-03 08:03:51", tz = "UTC"),
           y = 1.95,
           color = "#290AD8FF", label = "Bout 1, included", vjust = -1, size = 7) +
  annotate(geom = "segment", x = as.POSIXct("2000-01-03 08:03:48", tz = "UTC"),
           xend = as.POSIXct("2000-01-03 08:04:01", tz = "UTC"),
           y = 2.1,
           color = "#FFAD72FF", linewidth = 2) +
  annotate(geom = "label", x = as.POSIXct("2000-01-03 08:03:51", tz = "UTC"),
           y = 1.88, size = 7,
           color = "#FFAD72FF", label = "Bout 2, not included", vjust = -1)

p2 = day2 %>%
  filter(between(HEADER_TIMESTAMP, start, end)) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_rect(data = second_df %>% filter(between(HEADER_TIMESTAMP, start, end - as.period(1, "second"))),
            aes(xmin = HEADER_TIMESTAMP,
                xmax = HEADER_TIMESTAMP + as.period(1, "second"),
                ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#D82632FF", alpha = 0.5) +
  geom_line(linewidth = .5) +
  scale_x_datetime(date_breaks = "5 sec", date_labels =  "%H:%M:%S") +
  labs(x = "Time of day", y = "Acceleration (g)") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  annotate(geom = "segment", x = as.POSIXct("2000-01-03 08:03:36", tz = "UTC"),
           xend = as.POSIXct("2000-01-03 08:03:48", tz = "UTC"),
           y = 2,
           color = "#290AD8FF", linewidth = 2) +
  annotate(geom = "segment", x = as.POSIXct("2000-01-03 08:03:48", tz = "UTC"),
           xend = as.POSIXct("2000-01-03 08:04:01", tz = "UTC"),
           y = 2.1,
           color = "#FFAD72FF", linewidth = 2)

png(here::here("manuscript", "figs_final", "walking_segments_v2.png"), width = 6, height = 5, units = "in", res = 500)
p2
dev.off()
svg(here::here("manuscript", "figs_final", "walking_segments_v2.svg"))
p1
dev.off()

one_sec =
  day2 %>%
  filter(between(HEADER_TIMESTAMP, start, end)) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  mutate(second = floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
  filter(second == second[1])
library(paletteer)
p1 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  mutate(ind = row_number() / 80) %>%
  pivot_longer(cols = contains("vm")) %>%
  mutate(colr = factor(if_else(ind %in% c(.5, .7), 1, 0))) %>%
  mutate(name = factor(name, levels = c("vm", "lag_vm"), labels = c("Acceleration", "Lag 0.15s acceleration"))) %>%
  ggplot(aes(x = ind, y = value, color = name, linetype = name)) +
  geom_line() +
  geom_point() +
  # geom_vline(aes(xintercept = 0.5), linetype = "dotted", color = "red") +
  scale_color_manual(values = c("black", "darkgrey")) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank())) +
  annotate(geom = "point", x = 0.9, y =2.062, color = "#D55E00FF", size = 4.5) +
  annotate(geom = "point", x = 0.9, y = 0.689, color = "#D55E00FF", size = 4.5) +
  annotate(geom = "point", x = 0.7, y = 0.716, color = "#0072B2FF", size = 4.5) +
  annotate(geom = "point", x = 0.7, y = 0.809, color = "#0072B2FF", size = 4.5)

png(here::here("manuscript", "figs_final", "p1_v2.png"), width = 6, height = 2.5, units = "in", res = 500)
p1
dev.off()

p1 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  mutate(ind = row_number() / 80) %>%
  pivot_longer(cols = contains("vm")) %>%
  mutate(colr = factor(if_else(ind %in% c(.5, .7), 1, 0))) %>%
  mutate(name = factor(name, levels = c("vm", "lag_vm"), labels = c("Acceleration", "Lag 0.15s acceleration"))) %>%
  ggplot(aes(x = ind, y = value, color = name, linetype = name)) +
  geom_line() +
  geom_point() +
  # geom_vline(aes(xintercept = 0.5), linetype = "dotted", color = "red") +
  scale_color_manual(values = c("black", "darkgrey")) +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank())) +
  annotate(geom = "point", x = 0.9, y =2.062, color = "#D55E00FF", size = 4.5) +
  annotate(geom = "point", x = 0.9, y = 0.689, color = "#D55E00FF", size = 4.5) +
  annotate(geom = "point", x = 0.7, y = 0.716, color = "#0072B2FF", size = 4.5) +
  annotate(geom = "point", x = 0.7, y = 0.809, color = "#0072B2FF", size = 4.5)

svg(here::here("manuscript", "figs_final", "p1_legend.svg"))
p1
dev.off()

p2 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  mutate(ind = row_number() / 80) %>%
  # mutate(colr = factor(if_else(ind %in%c( .5, .7), 1, 0))) %>%
  ggplot(aes(x = vm, y = lag_vm)) +
  geom_point() +
  annotate(geom = "point", x = 2.062, y = .689, color = "#0072B2FF", size = 4.5) +
  annotate(geom = "point", x = .716, y = 0.809, color = "#D55E00FF", size = 4.5) +

  # scale_color_manual(values = c("black", "red")) +
  # scale_size_manual(values = c(1.5, 2.5)) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3)) +
  geom_vline(data = tibble(x = seq(0.25, 2.75, 0.25)), aes(xintercept = x), col = "darkgrey") +
  geom_hline(data = tibble(y = seq(0.25, 2.75, 0.25)), aes(yintercept = y), col = "darkgrey")

png(here::here("manuscript", "figs_final", "p2_v2.png"), width = 6, height = 2.5, units = "in", res = 350)
p2
dev.off()

extra = expand_grid(vm = seq(0, 3, 0.1), lag_vm = seq(0, 3, 0.1)) %>%
  mutate(vm = cut(vm, breaks=seq(0, 3, 0.25), include.lowest = TRUE),
         lag_vm = cut(lag_vm, breaks = seq(0, 3, 0.25), include.lowest = TRUE))  %>%
  mutate(n = 0,
         grp =paste0(vm, "_", lag_vm))

count_df =
  one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  drop_na() %>%
  mutate(vm = cut(vm, breaks=seq(0, 3, 0.25), include.lowest = TRUE),
         lag_vm = cut(lag_vm, breaks = seq(0, 3, 0.25), include.lowest = TRUE)) %>%
  group_by(vm, lag_vm) %>%
  count() %>%
  mutate(grp =paste0(vm, "_", lag_vm))
library(viridis)
plot_df =
  count_df %>%
  bind_rows(extra %>% filter(!(grp %in% count_df$grp)))

p3 = plot_df %>%
  ggplot(aes(x = vm, y = lag_vm, label = n)) +
  geom_tile(col = "black", aes(fill = n)) +
  scale_fill_viridis(limits = c(0.001, 12)) +
  geom_text(data = plot_df %>% filter(n > 0)) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16)) +
  # axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(x = "Acceleration range (g)", y = "Lag acceleration range (g)")

png(here::here("manuscript", "figs_final", "p3_v2.png"), width = 6, height = 2.5, units = "in", res = 350)
p3
dev.off()

png(here::here("manuscript", "figs", "grid_cells_v2.png"), width = 10, height = 10, units = "in", res = 300)
p1 / p2 / p3
dev.off()


## walking bouts


## pred figs
walking_dat = read_rds(here::here("data", "fingerprint_data_example.rds"))
ids = unique(walking_dat$id)
ids = ids[1:5]
walking_dat = walking_dat %>% filter(id %in% ids)
walking_dat$id = factor(walking_dat$id)

library(tidymodels)

second_df =
  walking_dat %>%
  select(id, second_id) %>%
  distinct()
splt = initial_split(second_df, strata = id, prop = 3/4)
train_secs = training(splt)
test_secs = testing(splt)


train =
  walking_dat %>%
  inner_join(train_secs, by = c("id", "second_id")) %>%
  mutate(data = "train")

test =
  walking_dat %>%
  inner_join(test_secs, by = c("id", "second_id")) %>%
  mutate(data = "test")


get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

library(ggnewscale)

dens_df =
  train %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df_12 =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  train %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 24)) %>%
  ungroup() %>%
  drop_na()

dens_df_24 =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  train %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 36)) %>%
  ungroup() %>%
  drop_na()

dens_df_36 =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

# dens_df %>%
#   ggplot(aes(x = vm, y = lag_vm, color = density)) +
#   geom_point(size = .5) +
#   scale_color_viridis() +
#   facet_grid(data~id)

true_sub12 = dens_df_12 %>%
  filter(id == 77035 & data == "train") %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = 1) +
  scale_color_viridis_b(option = "C") +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

true_sub24 = dens_df_24 %>%
  filter(id == 77035 & data == "train") %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = 1) +
  scale_color_viridis_b(option = "C") +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  theme(legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)")  +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))
true_sub36 = dens_df_36 %>%
  filter(id == 77035) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = 1) +
  scale_color_viridis_b(option = "C") +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

true_sub12 + true_sub24 + true_sub36

all_dens=
  dens_df_12 %>%
  mutate(lag = "12") %>%
  bind_rows(dens_df_24 %>% mutate(lag = "24")) %>%
  bind_rows(dens_df_36 %>% mutate(lag = "36")) %>%
  filter(id == "77035") %>%
  mutate(lag = factor(lag, labels = c("Lag = 0.15s", "Lag = 0.30s", "Lag = 0.45s")),
         id = "Participant 1")


p <- ggplot()

# Get unique facet combinations
facet_levels <- all_dens %>% distinct(id, lag)

# Loop over each facet and add a layer
for (i in seq_len(nrow(facet_levels))) {
  this_id <- facet_levels$id[i]
  this_data <- facet_levels$lag[i]

  # Filter data for this facet
  this_df <- all_dens %>% filter(id == this_id, lag == this_data)

  # Add the layer
  p <- p +
    geom_point(
      data = this_df,
      aes(x = vm, y = lag_vm, color = density),
      size = 1
    ) +
    scale_color_viridis_b(option = "C") +
    new_scale_color()
}

# Add faceting after all layers
p1 = p + facet_grid(lag ~ id) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)", title = "Training Data") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

####

dens_df =
  test %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df_12a =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  test %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 24)) %>%
  ungroup() %>%
  drop_na()

dens_df_24a =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

dens_df =
  test %>%
  group_by(id, second_id, data) %>%
  mutate(lag_vm = lag(vm, n = 36)) %>%
  ungroup() %>%
  drop_na()

dens_df_36a =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80))) %>%
  ungroup()

all_densa =
  dens_df_12a %>%
  mutate(lag = "12") %>%
  bind_rows(dens_df_24a %>% mutate(lag = "24")) %>%
  bind_rows(dens_df_36a %>% mutate(lag = "36")) %>%
  mutate(id = fct_relevel(id, "77035", after = 0L)) %>%
  mutate(id = paste0("Participant ", as.factor(as.numeric(id)))) %>%
  mutate(id = case_when(
    id == "Participant 1" ~ "Participant A",
    id == "Participant 2" ~ "Participant B",
    id == "Participant 3" ~ "Participant C",
    id == "Participant 4" ~ "Participant D",
    .default =  "Participant E"
  )) %>%
  mutate(lag = factor(lag, labels = c("Lag = 0.15s", "Lag = 0.30s", "Lag = 0.45s")))

p <- ggplot()

# Get unique facet combinations
facet_levels <- all_densa %>% distinct(id, lag)

# Loop over each facet and add a layer
for (i in seq_len(nrow(facet_levels))) {
  this_id <- facet_levels$id[i]
  this_data <- facet_levels$lag[i]

  # Filter data for this facet
  this_df <- all_densa %>% filter(id == this_id, lag == this_data)

  # Add the layer
  p <- p +
    geom_point(
      data = this_df,
      aes(x = vm, y = lag_vm, color = density),
      size = 1
    ) +
    scale_color_viridis_b(option= "C") +
    new_scale_color()
}

preds = tibble(
  id = paste("Participant ", c("A", "B", "C", "D", "E"), sep = ""),
  probs = c(0.101, 0.008,  0.007, 0.0070, .006),
  lag = rep("Lag = 0.15s", 5)
)
# Add faceting after all layers
p2 = p + facet_grid(lag ~ id) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)", title = "Testing Data") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3)) +
  geom_label(data = preds,
             aes(x = 2, y = 2.6, label = paste0("p = ", probs)), size = 3)


p_final = p1 + p2 + plot_layout(widths = c(1, 6), axes = "collect")
png(here::here("manuscript", "figs_final", "fingerprint_predictions.png"), width = 10, height = 6, units = "in",
    res = 350)
p1 + p2 + plot_layout(widths = c(1, 6), axes = "collect")
dev.off()


####
library(tidyverse)
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}




# dens_df$density = get_density(dens_df$vm, dens_df$lag_vm, n = 100)
sample_dat = read_rds(here::here("data", "fingerprint_data_sample.rds"))
ids = unique(sample_dat$id)

dens_df =
  sample_dat %>%
  group_by(id, second) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df =
  dens_df %>%
  ungroup() %>%
  group_by(id) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80)))

dens_df %>%
  filter(id %in% ids[1:10]) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point() +
  scale_color_viridis() +
  facet_wrap(.~id)

sample_dat2 = read_rds(here::here("data", "fingerprint_data_sample_temporal.rds"))
ids = unique(sample_dat2$id)

dens_df =
  sample_dat2 %>%
  group_by(id, second) %>%
  mutate(lag_vm = lag(vm, n = 12)) %>%
  ungroup() %>%
  drop_na()
dens_df =
  dens_df %>%
  ungroup() %>%
  group_by(id, data) %>%
  group_modify(~ .x %>% mutate(density = get_density(vm, lag_vm, n = 80)))

# purrr::walk(seq(1, 96, 5), \(x){
#   p = dens_df %>%
#     filter(id %in% ids[x:(x+4)]) %>%
#     ggplot(aes(x = vm, y = lag_vm, color = density)) +
#     geom_point() +
#     scale_color_viridis() +
#     facet_grid(data~id)
#   print(p)
# })
# dens_df %>%
#   filter(id %in% ids[20:25]) %>%
#   ggplot(aes(x = vm, y = lag_vm, color = density)) +
#   geom_point() +
#   scale_color_viridis() +
#   facet_grid(data~id)


p1 = dens_df %>%
  filter(id == 78898) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = .85) +
  scale_color_viridis_b(name = "# points", option = "C") +
  facet_grid(data~.) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  scale_y_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  theme(legend.position = c(0.9, 0.7),
        panel.grid = element_blank())
p1
p2 = sample_dat2 %>%
  filter(id == 78898) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  group_by(data) %>%
  mutate(row = row_number()) %>%
  filter(row <= 800) %>%
  ungroup() %>%
  ggplot(aes(x = row, y = vm)) +
  geom_line() +
  facet_grid(data~.) +
  scale_x_continuous(breaks=seq(0, 800, 80), labels = seq(0, 10, 1)) +
  scale_y_continuous(limits=c(0, 2.5)) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())

library(patchwork)

p_1 = p2 + p1 + plot_annotation(title = "Subject 1")

# other subs to investigate: 68561, 70844, 67407, 68022, 82860, 69972, 81349, 78898

p3 = dens_df %>%
  filter(id == 68561) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  ggplot(aes(x = vm, y = lag_vm, color = density)) +
  geom_point(size = .85) +
  scale_color_viridis_b(option = "C", name = "# points") +
  facet_grid(data~.) +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  scale_y_continuous(limits=c(0,3), breaks=seq(0, 3, 0.5)) +
  theme(legend.position = c(0.9, 0.7),
        panel.grid = element_blank())

p4 = sample_dat2 %>%
  filter(id == 68561) %>%
  mutate(data = factor(data, levels = c("train", "test"), labels = c("Training data", "Testing data"))) %>%
  group_by(data) %>%
  mutate(row = row_number()) %>%
  filter(row <= 800) %>%
  ungroup() %>%
  ggplot(aes(x = row, y = vm)) +
  geom_line() +
  facet_grid(data~.) +
  scale_x_continuous(breaks=seq(0, 800, 80), labels = seq(0, 10, 1)) +
  scale_y_continuous(limits=c(0,2.5)) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())

library(patchwork)
p_2 = (p4 + p3) + plot_annotation(title = "Subject 2")
png(here::here("manuscript", "figs_final", "fingerprint_data.png"), width = 10, height = 8, units = "in",
    res = 350)
p_1 / p_2 + plot_annotation(tag_levels = "A")
dev.off()

(p2 + p1) / (p4 + p3)



