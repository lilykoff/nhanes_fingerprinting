library(tidyverse)
library(ggplot2)
options(digits.secs = 3)
library(patchwork)
library(adept)
library(adeptdata)
theme_set(theme_light(base_size = 12))
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

p1 = day2 %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_line(linewidth = .05) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(panel.grid = element_blank()) +
  labs(x = "Time of day", y = "Acceleration (g)")


all_wrist_templates = adeptdata::stride_template$left_wrist
template_list = do.call(rbind, all_wrist_templates)
template_list = apply(template_list, 1, identity, simplify = FALSE)
template = template_list

# data = bind_rows(day1, day2)
data = day2
step_result = adept::segmentWalking(
  xyz = data[, c("X", "Y", "Z")],
  xyz.fs = 80,
  template = template_list,
  compute.template.idx = FALSE,
  run.parallel = TRUE,
  run.parallel.cores = 8,
  sim_MIN = 0.6,
  dur_MIN = 0.8,
  dur_MAX = 1.4,
  ptp_r_MIN = 0.5,
  ptp_r_MAX = 2,
  vmc_r_MIN = 0.05,
  vmc_r_MAX = 0.5,
  mean_abs_diff_med_p_MAX = 0.7,
  mean_abs_diff_med_t_MAX = 0.2,
  mean_abs_diff_dur_MAX = 0.3
) %>%
  filter(is_walking_i == 1) %>%
  mutate(steps = 2 / (T_i / 80))

write_rds(step_result, here::here("data", "plot_df_step_result.rds"))


# the steps per second
steps_seconds =
  data %>%
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
  filter(HEADER_TIMESTAMP >= start & HEADER_TIMESTAMP <= start + as.period(1, "hour"))
p2 = day2 %>%
  filter(HEADER_TIMESTAMP >= start & HEADER_TIMESTAMP <= start + as.period(1, "hour")) %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_rect(data = second_df,
            aes(xmin = HEADER_TIMESTAMP,
                xmax = HEADER_TIMESTAMP + as.period(1, "second"),
                ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "red") +
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
                ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "red", alpha = 0.5) +
  geom_line(linewidth = .5) +
  scale_x_datetime(date_breaks = "5 sec", date_labels =  "%H:%M:%S") +
  labs(x = "Time of day", y = "Acceleration (g)") +
  theme(panel.grid = element_blank())
png(here::here("manuscript", "figs", "figure1_3panel.png"), width = 6, height = 8, units = "in",
    res = 350)
p1 / p2 / p3 + plot_annotation(tag_levels = "A")
dev.off()
p1 = day1 %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_line(linewidth = .1) +
  # scale_y_continuous(limits=c(0,4)) +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  # labs(x = "Time of day", y = "Acceleration (g)", title = "First day of observation for one participant") +
  labs(x = "Time of day", y = "Acceleration (g)", title = "Day 1, participant 1")


p2 =
  day2 %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_line(linewidth = .1) +
  # scale_y_continuous(limits=c(0,4)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H") +
  # theme(axis.text.x = element_blank()) +
  labs(x = "Time of day", y = "Acceleration (g)", title = "Day 2")


png(here::here("manuscript", "figs", "day1_day2.png"), width = 10, height = 5, units = "in", res = 300)
p1 + p2
dev.off()

png(here::here("manuscript", "figs", "day1.png"), width = 3.5, height = 2, units = "in", res = 500)
p1
dev.off()



all_wrist_templates = adeptdata::stride_template$left_wrist
template_list = do.call(rbind, all_wrist_templates)
template_list = apply(template_list, 1, identity, simplify = FALSE)
template = template_list

# data = bind_rows(day1, day2)
data = day1
step_result = adept::segmentWalking(
  xyz = data[, c("X", "Y", "Z")],
  xyz.fs = 80,
  template = template_list,
  compute.template.idx = FALSE,
  run.parallel = TRUE,
  run.parallel.cores = 8,
  sim_MIN = 0.6,
  dur_MIN = 0.8,
  dur_MAX = 1.4,
  ptp_r_MIN = 0.5,
  ptp_r_MAX = 2,
  vmc_r_MIN = 0.05,
  vmc_r_MAX = 0.5,
  mean_abs_diff_med_p_MAX = 0.7,
  mean_abs_diff_med_t_MAX = 0.2,
  mean_abs_diff_dur_MAX = 0.3
) %>%
  filter(is_walking_i == 1) %>%
  mutate(steps = 2 / (T_i / 80))
# the steps per second
steps_seconds =
  data %>%
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
second_df =
  steps_seconds %>%
  mutate(HEADER_TIMESTAMP = time)
p3 = day1 %>%
  mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
  ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
  geom_line(linewidth = .1) +
  geom_rect(data = second_df,
            aes(xmin = HEADER_TIMESTAMP,
                xmax = HEADER_TIMESTAMP + as.period(1, "second"),
            ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "red") +
  # scale_y_continuous(limits=c(0,4)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H") +
  theme(panel.grid = element_blank()) +
  # theme(axis.text.x = element_blank()) +
  labs(x = "Time (hours)", y = "Acceleration (g)", title = "Day 1")

png(here::here("manuscript", "figs", "day1_highlighted.png"), width = 10, height = 5, units = "in", res = 300)
p3
dev.off()


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

  # key of those times
  seconds_key =
    segments_10 %>%
    group_by(rleid) %>%
    tidyr::expand(second = seq(start, end, "sec"))

  p = day1 %>%
    mutate(hour = hour(HEADER_TIMESTAMP),
           minute = minute(HEADER_TIMESTAMP)) %>%
    filter(hour %in% 15 & minute >= 30)  %>%
    mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
    ggplot(aes(x = HEADER_TIMESTAMP, y = vm)) +
    # geom_rect(data = seconds_key %>% rename(HEADER_TIMESTAMP = second) %>%
    #             mutate(vm=1, hour = hour(HEADER_TIMESTAMP)),
    #           aes(xmin = HEADER_TIMESTAMP, xmax = HEADER_TIMESTAMP + as.period(1, "second"),
    #               ymin = -Inf, ymax = Inf), color = "yellow") +
    geom_rect(data = tibble(min = c(as.POSIXct("2000-01-02 15:38:00", tz = "UTC"),
                                    as.POSIXct("2000-01-02 15:52:00", tz = "UTC"))),
      aes(xmin = min,xmax = min + as.period(15, "second"),
                  ymin = -Inf, ymax = Inf), fill = "yellow", inherit.aes = FALSE) +
    geom_line(linewidth = .5) +
    labs(x = "Time of day", y = "Acceleration (g)", title = "30 minutes with two walking bouts") +
    theme(panel.grid = element_blank())

  png(here::here("manuscript", "figs", "30min.png"), width = 4, height = 2.5, units = "in", res = 500)
  p
  dev.off()

  p_excl =
    day1 %>%
    mutate(second = floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
    filter(second %in% seconds_key$second) %>%
    mutate(index = (row_number() -1) / 80) %>%
    left_join(steps_seconds, by = c("second" = "time")) %>%
    mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
    mutate(steps = if_else(!is.na(steps_adept), "Steps > 0", "No steps")) %>%
    mutate(steps = if_else(second == second[83], "No steps", steps))  %>%
    ggplot(aes(x = index, y = vm, color = steps, group = second)) +
    geom_line(linewidth = .5) +
    scale_x_continuous(breaks=seq(0,11,1)) +
    theme(panel.grid = element_blank(),
          legend.position = c(0.8, 0.8)) +
    geom_vline(data = tibble(xint = seq(0,11,1)), aes(xintercept = xint), color = "grey") +
    labs(x = "Time (sec)", y = "Acceleration (g)", title = "Bout 1, not included") +
    scale_color_manual(values = c("lightblue", "black"), name = "")

  p_excl2 =
    day1 %>%
    mutate(second = floor_date(HEADER_TIMESTAMP, unit = "seconds")) %>%
    filter(second %in% seconds_key$second) %>%
    mutate(index = (row_number() -1) / 80) %>%
    left_join(steps_seconds, by = c("second" = "time")) %>%
    mutate(vm = sqrt(X^2 + Y^2 + Z^2)) %>%
    mutate(steps = if_else(!is.na(steps_adept), "Steps > 0", "No steps")) %>%
    mutate(steps = if_else(second == second[83], "No steps", steps))  %>%
    ggplot(aes(x = index, y = vm, color = steps, group = second)) +
    geom_line(linewidth = .5) +
    scale_x_continuous(breaks=seq(0,11,1)) +
    theme(panel.grid = element_blank(),
          legend.position = "none") +
    geom_vline(data = tibble(xint = seq(0,11,1)), aes(xintercept = xint), color = "grey") +
    labs(x = "Time (sec)", y = "Acceleration (g)", title = "Bout 1, not included") +
    scale_color_manual(values = c("lightblue", "black"), name = "")


  sub = "70024"
  sub1 = read_csv(here::here("data", "walking_samples", paste0(sub, ".csv.gz")))
  segments_10 = sub1 %>%
    select(second) %>%
    distinct() %>%
    mutate(
      timediff = as.numeric(difftime(second, dplyr::lag(second, n = 1), units = "secs")),
      ltwosec = (timediff <= 2) * 1,
      rleid = data.table::rleid(ltwosec)
    ) %>%
    filter(ltwosec == 1) %>%
    group_by(rleid) %>%
    summarize(
      n_seconds = n(),
      start = min(second),
      end = max(second)
    ) %>%
    filter(n_seconds >= 10)

  # key of those times
  seconds_key =
    segments_10 %>%
    group_by(rleid) %>%
    tidyr::expand(second = seq(start, end, "sec"))

  df_small =
    sub1 %>%
    inner_join(seconds_key, by = c("second"))

  rls = unique(df_small$rleid)

  p4 = df_small %>%
    filter(rleid %in% rls[1:4]) %>%
    group_by(rleid) %>%
    mutate(index = row_number() / 80) %>%
    ggplot(aes(x = index, y = vm)) +
    geom_line(linewidth = .5) +
    facet_wrap(rleid ~ ., nrow = 1, scales = "free_x") +
    labs(
      x = "Time (sec)",
      y = "Acceleration (g)") +
    theme(strip.text = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(breaks=seq(0,15,2))



  png(here::here("manuscript", "figs", "walking_segments.png"), width = 10, height = 5, units = "in", res = 300)
  p4
  dev.off()

  p4 = df_small %>%
    filter(rleid %in% rls[1]) %>%
    group_by(rleid) %>%
    mutate(index = row_number() / 80) %>%
    ggplot(aes(x = index, y = vm)) +
    geom_line(linewidth = .5) +
    scale_y_continuous(limits=c(0.5,2.5)) +
    facet_wrap(rleid ~ ., nrow = 1, scales = "free_x") +
    labs(
      x = "Time (sec)",
      y = "Acceleration (g)") +
    theme(strip.text = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(breaks=seq(0,15,2))


  png(here::here("manuscript", "figs", "walking_segments1.png"), width = 10, height = 5, units = "in", res = 300)
  p4
  dev.off()

  p4 = df_small %>%
    filter(rleid %in% rls[2]) %>%
    group_by(rleid) %>%
    mutate(index = row_number() / 80) %>%
    ggplot(aes(x = index, y = vm)) +
    scale_y_continuous(limits=c(0.5,2.5)) +
    geom_line(linewidth = .5) +
    facet_wrap(rleid ~ ., nrow = 1, scales = "free_x") +
    labs(
      x = "Time (sec)",
      y = "Acceleration (g)") +
    theme(strip.text = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(breaks=seq(0,15,2))

  png(here::here("manuscript", "figs", "walking_segments2.png"), width = 10, height = 5, units = "in", res = 300)
  p4
  dev.off()

  p4 = df_small %>%
    filter(rleid %in% rls[3]) %>%
    group_by(rleid) %>%
    mutate(index = row_number() / 80) %>%
    ggplot(aes(x = index, y = vm)) +
    scale_y_continuous(limits=c(0.5,2.5)) +
    geom_line(linewidth = .5) +
    facet_wrap(rleid ~ ., nrow = 1, scales = "free_x") +
    labs(
      x = "Time (sec)",
      y = "Acceleration (g)") +
    theme(strip.text = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(breaks=seq(0,15,2))

  png(here::here("manuscript", "figs", "walking_segments3.png"), width = 10, height = 5, units = "in", res = 300)
  p4
  dev.off()

p5 = df_small %>%
  filter(rleid == rls[1]) %>%
  group_by(rleid) %>%
  mutate(index = (row_number()-1) / 80) %>%
  ggplot(aes(x = index, y = vm)) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf), fill = "yellow") +
  geom_line(linewidth = .9) +
  geom_vline(data = tibble(index = seq(0, 11, 1)),
                           aes(xintercept = index), color = "grey") +
  theme(panel.grid = element_blank()) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  scale_x_continuous(breaks = seq(0, 11, 1))

png(here::here("manuscript", "figs", "seconds.png"), width = 10, height = 5, units = "in", res = 300)
p5
dev.off()

pincl = df_small %>%
  filter(rleid == rls[1]) %>%
  group_by(rleid) %>%
  mutate(index = (row_number()-1) / 80) %>%
  ggplot(aes(x = index, y = vm)) +
  # geom_rect(aes(xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf), fill = "yellow") +
  geom_line(linewidth = .5) +
  geom_vline(data = tibble(index = seq(0, 11, 1)),
             aes(xintercept = index), color = "grey") +
  theme(panel.grid = element_blank()) +
  labs(x = "Time (sec)", y = "Acceleration (g)", title = "Bout 2, included") +
  scale_x_continuous(breaks = seq(0, 11, 1))

png(here::here("manuscript", "figs", "seconds_inc.png"), width = 2.5, height = 2.5, units = "in", res = 500)
pincl
dev.off()

png(here::here("manuscript", "figs", "seconds_excl.png"), width = 2.5, height = 2.5, units = "in", res = 500)
p_excl2
dev.off()

svg(here::here("manuscript", "figs", "seconds_excl.svg"), width = 2.5, height = 2.5)
p_excl
dev.off()

one_sec =
  df_small %>%
  filter(rleid == rls[1])  %>%
  ungroup() %>%
  filter(second == second[1])
library(paletteer)
p1 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  mutate(ind = row_number() / 80) %>%
  pivot_longer(cols = contains("vm")) %>%
  mutate(name = factor(name, levels = c("vm", "lag_vm"), labels = c("Acceleration", "Lag 0.15s acceleration"))) %>%
  ggplot(aes(x = ind, y = value, color = name, linetype = name)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = 0.5), linetype = "dotted", color = "red") +
  scale_color_manual(values = c("#1E8E99FF", "#CC5800FF")) +
  theme(legend.position = "bottom") +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank()))

p2 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  mutate(ind = row_number() / 80) %>%
  mutate(colr = factor(if_else(ind == .5, 1, 0))) %>%
  ggplot(aes(x = vm, y = lag_vm)) +
  geom_point(size = 1.5, aes(col = colr)) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

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

plot_df =
  count_df %>%
  bind_rows(extra %>% filter(!(grp %in% count_df$grp)))

p3 = plot_df %>%
  ggplot(aes(x = vm, y = lag_vm, label = n)) +
  geom_tile(col = "black", aes(fill = n)) +
  scale_fill_viridis(limits = c(0.001, 12)) +
  geom_text(data = plot_df %>% filter(n > 0)) +
  theme(legend.position = "none") +
  labs(x = "Acceleration range (g)", y = "Lag acceleration range (g)")

png(here::here("manuscript", "figs", "grid_cells.png"), width = 10, height = 10, units = "in", res = 300)
p1 / p2 / p3
dev.off()



p1l = one_sec %>%
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
  theme(legend.position = "bottom") +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank())) +
  annotate(geom = "point", x = 0.5, y = 0.984, color = "red", size = 2) +
  annotate(geom = "point", x = 0.5, y = 0.761, color = "red", size = 2) +
  annotate(geom = "point", x = 0.7, y = 0.981, color = "darkblue", size = 2) +
  annotate(geom = "point", x = 0.7, y = 1.27, color = "darkblue", size = 2)

svg(here::here("manuscript", "figs", "legend.svg"), width = 5, height = 5)
p1l
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
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank())) +
  annotate(geom = "point", x = 0.5, y = 0.984, color = "#D55E00FF", size = 2.5) +
  annotate(geom = "point", x = 0.5, y = 0.761, color = "#D55E00FF", size = 2.5) +
  annotate(geom = "point", x = 0.7, y = 0.981, color = "#0072B2FF", size = 2.5) +
  annotate(geom = "point", x = 0.7, y = 1.27, color = "#0072B2FF", size = 2.5)

png(here::here("manuscript", "figs", "p1.png"), width = 6, height = 2.5, units = "in", res = 500)
p1
dev.off()

p2 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 12L)) %>%
  mutate(ind = row_number() / 80) %>%
  # mutate(colr = factor(if_else(ind %in%c( .5, .7), 1, 0))) %>%
  ggplot(aes(x = vm, y = lag_vm)) +
  geom_point() +
  annotate(geom = "point", y = 1.27, x = 0.981, color = "#0072B2FF", size = 2.5) +
  annotate(geom = "point", y = .761, x = 0.984, color = "#D55E00FF", size = 2.5) +

  # scale_color_manual(values = c("black", "red")) +
  # scale_size_manual(values = c(1.5, 2.5)) +
  theme(legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3)) +
  geom_vline(data = tibble(x = seq(0.25, 2.75, 0.25)), aes(xintercept = x), col = "darkgrey") +
  geom_hline(data = tibble(y = seq(0.25, 2.75, 0.25)), aes(yintercept = y), col = "darkgrey")

png(here::here("manuscript", "figs", "p2.png"), width = 6, height = 2.5, units = "in", res = 500)
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
        axis.ticks = element_blank()) +
        # axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(x = "Acceleration range (g)", y = "Lag acceleration range (g)")

png(here::here("manuscript", "figs", "p3.png"), width = 6, height = 2.5, units = "in", res = 500)
p3
dev.off()

png(here::here("manuscript", "figs", "grid_cells.png"), width = 10, height = 10, units = "in", res = 300)
p1 / p2 / p3
dev.off()




p1 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 24L)) %>%
  mutate(ind = row_number() / 80) %>%
  pivot_longer(cols = contains("vm")) %>%
  mutate(name = factor(name, levels = c("vm", "lag_vm"), labels = c("Acceleration", "Lag 0.15s acceleration"))) %>%
  ggplot(aes(x = ind, y = value, color = name, linetype = name)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = 0.5), linetype = "dotted", color = "red") +
  scale_color_manual(values = c("#1E8E99FF", "#CC5800FF")) +
  theme(legend.position = "bottom") +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank()))

p2 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 24L)) %>%
  mutate(ind = row_number() / 80) %>%
  mutate(colr = factor(if_else(ind == .5, 1, 0))) %>%
  ggplot(aes(x = vm, y = lag_vm)) +
  geom_point(size = 1.5, aes(col = colr)) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

extra = expand_grid(vm = seq(0, 3, 0.1), lag_vm = seq(0, 3, 0.1)) %>%
  mutate(vm = cut(vm, breaks=seq(0, 3, 0.25), include.lowest = TRUE),
         lag_vm = cut(lag_vm, breaks = seq(0, 3, 0.25), include.lowest = TRUE))  %>%
  mutate(n = 0,
         grp =paste0(vm, "_", lag_vm))

count_df =
  one_sec %>%
  mutate(lag_vm = lag(vm, n = 24L)) %>%
  drop_na() %>%
  mutate(vm = cut(vm, breaks=seq(0, 3, 0.25), include.lowest = TRUE),
         lag_vm = cut(lag_vm, breaks = seq(0, 3, 0.25), include.lowest = TRUE)) %>%
  group_by(vm, lag_vm) %>%
  count() %>%
  mutate(grp =paste0(vm, "_", lag_vm))

plot_df =
  count_df %>%
  bind_rows(extra %>% filter(!(grp %in% count_df$grp)))

p3 = plot_df %>%
  ggplot(aes(x = vm, y = lag_vm, label = n)) +
  geom_tile(col = "black", aes(fill = n)) +
  scale_fill_viridis(limits = c(0.001, 16)) +
  geom_text(data = plot_df %>% filter(n > 0)) +
  theme(legend.position = "none") +
  labs(x = "Acceleration range (g)", y = "Lag acceleration range (g)")

p1 / p2 / p3

p1 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 36L)) %>%
  mutate(ind = row_number() / 80) %>%
  pivot_longer(cols = contains("vm")) %>%
  mutate(name = factor(name, levels = c("vm", "lag_vm"), labels = c("Acceleration", "Lag 0.15s acceleration"))) %>%
  ggplot(aes(x = ind, y = value, color = name, linetype = name)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = 0.5), linetype = "dotted", color = "red") +
  scale_color_manual(values = c("#1E8E99FF", "#CC5800FF")) +
  theme(legend.position = "bottom") +
  labs(x = "Time (sec)", y = "Acceleration (g)") +
  guides(color = guide_legend(title = element_blank()),
         linetype = guide_legend(title = element_blank()))

p2 = one_sec %>%
  mutate(lag_vm = lag(vm, n = 36L)) %>%
  mutate(ind = row_number() / 80) %>%
  mutate(colr = factor(if_else(ind == .5, 1, 0))) %>%
  ggplot(aes(x = vm, y = lag_vm)) +
  geom_point(size = 1.5, aes(col = colr)) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none") +
  labs(x = "Acceleration (g)", y = "Lag Acceleration (g)") +
  scale_x_continuous(limits=c(0,3)) +
  scale_y_continuous(limits=c(0,3))

extra = expand_grid(vm = seq(0, 3, 0.1), lag_vm = seq(0, 3, 0.1)) %>%
  mutate(vm = cut(vm, breaks=seq(0, 3, 0.25), include.lowest = TRUE),
         lag_vm = cut(lag_vm, breaks = seq(0, 3, 0.25), include.lowest = TRUE))  %>%
  mutate(n = 0,
         grp =paste0(vm, "_", lag_vm))

count_df =
  one_sec %>%
  mutate(lag_vm = lag(vm, n = 36L)) %>%
  drop_na() %>%
  mutate(vm = cut(vm, breaks=seq(0, 3, 0.25), include.lowest = TRUE),
         lag_vm = cut(lag_vm, breaks = seq(0, 3, 0.25), include.lowest = TRUE)) %>%
  group_by(vm, lag_vm) %>%
  count() %>%
  mutate(grp =paste0(vm, "_", lag_vm))

plot_df =
  count_df %>%
  bind_rows(extra %>% filter(!(grp %in% count_df$grp)))

p3 = plot_df %>%
  ggplot(aes(x = vm, y = lag_vm, label = n)) +
  geom_tile(col = "black", aes(fill = n)) +
  scale_fill_viridis(limits = c(0.001, 16)) +
  geom_text(data = plot_df %>% filter(n > 0)) +
  theme(legend.position = "none") +
  labs(x = "Acceleration range (g)", y = "Lag acceleration range (g)")

p1 / p2 / p3
