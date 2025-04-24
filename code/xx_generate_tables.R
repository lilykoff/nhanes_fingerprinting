library(tidyverse)
library(glue)
library(paletteer)
library(patchwork)
theme_set(theme_light(base_size = 12))
covars = read_rds(here::here("data", "covariates_accel_mortality_df.rds"))
covars_y = read_rds(here::here("data", "covariates_accel_df_paxy.rds"))
library(gt)


all_subs = read_rds(here::here("data", "fingerprint_folds.rds"))
temporal_subs = read_rds(here::here("data", "fingerprint_folds_temporal2.rds"))

### population summaries
df =
  covars %>%
  bind_rows(covars_y)  %>%
  filter(has_accel) %>%
  mutate(data_release_cycle = if_else(is.na(data_release_cycle), "NYFFS", as.character(data_release_cycle))) %>%
  mutate(random = SEQN %in% all_subs$id,
         temporal = SEQN %in% temporal_subs$id)


t1 =
  df %>%
  select(data_release_cycle, gender, age = age_in_years_at_screening,
         race = race_hispanic_origin, weight_kg, standing_height_cm, body_mass_index_kg_m_2) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Overall**")

t2 =
  df %>%
  filter(random) %>%
  select(data_release_cycle, gender, age = age_in_years_at_screening,
         race = race_hispanic_origin, weight_kg, standing_height_cm, body_mass_index_kg_m_2) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Random**")

t3 =
  df %>%
  filter(temporal) %>%
  select(data_release_cycle, gender, age = age_in_years_at_screening,
         race = race_hispanic_origin, weight_kg, standing_height_cm, body_mass_index_kg_m_2) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Temporal**")

tbl_merge(
  tbls = list(t1, t2, t3),
  tab_spanner = c("**All participants with acceleromtery**", "**Random Paradigm**", "**Temporal Paradigm**")
) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

### accuracies for n=100,n=500

res_100 = c(list.files(here::here("results"), pattern = "*.\\_100.rds"),
            list.files(here::here("results"), pattern = "*.\\_100fnl.rds"),
            list.files(here::here("results"), pattern = "*.\\_100nlfnl.rds"),
            list.files(here::here("results"), pattern = "*.\\_100lasso.rds"),
            list.files(here::here("results"), pattern = "*.\\_100xgb.rds"),
            list.files(here::here("results"), pattern = "*.\\_100rf.rds"))

all_100 =
  map_dfr(res_100,
          .f = function(x){
            read_rds(here::here("results", x)) %>%
              mutate(name = x)
          }) %>%
  mutate(
    type = case_when(
      sub(".*\\d(.+).rds.*", "\\1", name) == "xgb" ~ "XGBoost",
      sub(".*\\d(.+).rds.*", "\\1", name) == "long" ~ "Long",
      sub(".*\\d(.+).rds.*", "\\1", name) == "lasso" ~ "Lasso",
      sub(".*\\d(.+).rds.*", "\\1", name) == "rf" ~ "Random Forest",
      sub(".*\\d(.+).rds.*", "\\1", name) == "fnl" ~ "Linear SoFR",
      sub(".*\\d(.+).rds.*", "\\1", name) == "nlfnl" ~ "Nonlinear SoFR",
      .default = "Logistic"
    ),
    paradigm = case_when(
      grepl("temporal2", name) ~ "temporal2",
      grepl("temporal", name) ~ "temporal",
      .default = "random"
    )
  ) %>%
  filter(paradigm != "temporal")


res_500 = c(list.files(here::here("results"), pattern = "*.\\_500.rds"),
            list.files(here::here("results"), pattern = "*.\\_500fnl.rds"),
            list.files(here::here("results"), pattern = "*.\\_500nlfnl.rds"),
            list.files(here::here("results"), pattern = "*.\\_500lasso.rds"))

all_500 =
  map_dfr(res_500,
          .f = function(x){
            read_rds(here::here("results", x)) %>%
              mutate(name = x) %>%
              mutate(fold = as.numeric(fold))
          }) %>%
  mutate(
    type = case_when(
      sub(".*\\d(.+).rds.*", "\\1", name) == "xgb" ~ "XGBoost",
      sub(".*\\d(.+).rds.*", "\\1", name) == "long" ~ "Long",
      sub(".*\\d(.+).rds.*", "\\1", name) == "lasso" ~ "Lasso",
      sub(".*\\d(.+).rds.*", "\\1", name) == "rf" ~ "Random Forest",
      sub(".*\\d(.+).rds.*", "\\1", name) == "fnl" ~ "Linear SoFR",
      sub(".*\\d(.+).rds.*", "\\1", name) == "nlfnl" ~ "Nonlinear SoFR",
      .default = "Logistic"
    ),
    paradigm = case_when(
      grepl("temporal2", name) ~ "temporal2",
      grepl("temporal", name) ~ "temporal",
      .default = "random"
    )
  ) %>%
  filter(paradigm != "temporal")


all_100 %>%
  bind_rows(all_500 %>% mutate(fold = as.character(fold))) %>%
  mutate(across(c(rank1, rank5), ~(.x / n)*100)) %>%
  # filter(n %in% c(100, 500) & type != "Long") %>%
  group_by(paradigm, type, n) %>%
  summarize(across(c(rank1, rank5), list(med = median, min = min, max = max)), .groups = "drop") %>%
  group_by(paradigm) %>%
  arrange(desc(rank1_med), .by_group = TRUE) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = glue('{rank1_med} [{rank1_min},{rank1_max}]'),
         r5 = glue('{rank5_med} [{rank5_min},{rank5_max}]')) %>%
  select(paradigm, type, n, r1, r5) %>%
  pivot_wider(names_from = n, values_from = c(r1, r5)) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# paletteer_d("colorBlindness::paletteMartin")

p = all_100 %>%
  bind_rows(all_500 %>% mutate(fold = as.character(fold))) %>%
  mutate(across(c(rank1, rank5), ~(.x / n)*100)) %>%
  mutate(n = factor(n, labels = c("n = 100", "n = 500"))) %>%
  group_by(paradigm) %>%
  mutate(type = factor(type, levels = c("Logistic", "Lasso", "XGBoost", "Random Forest", "Nonlinear SoFR", "Linear SoFR")),
         paradigm = factor(paradigm, levels = c("random", "temporal2"), labels = c("Random", "Temporal"))) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = metric, y = value, color = type)) +
  geom_boxplot(outlier.size = .5, outlier.alpha = .5) +
  scale_color_manual(values = c("#000000FF", "#009292FF", "#DB6D00FF", "#006DDBFF", "#B66DFFFF", "#920000FF"), name = "Model") +
  # geom_point(position = position_jitterdodge(), alpha = .2, size = .2) +
  facet_grid(paradigm~n, scales = "free_x") +
  theme(legend.position = c(0.75, 0.35),
        panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Accuracy") +
  scale_x_discrete(labels = c("Rank 1", "Rank 5")) +
  guides(color = guide_legend(nrow = 2))


png(here::here("manuscript", "figs", "acc100_500.png"), width = 6, height = 4, units = "in",
    res = 100)
print(p)
dev.off()

res_logistic = c(list.files(here::here("results"), pattern = "*.\\_100.rds"),
            list.files(here::here("results"), pattern = "*.\\_500.rds"),
            list.files(here::here("results"), pattern = "*.\\_1000.rds"),
            list.files(here::here("results"), pattern = "*.\\_2500.rds"),
            list.files(here::here("results"), pattern = "*.\\_5000.rds"),
            list.files(here::here("results"), pattern = "*.\\_10000.rds"),
            list.files(here::here("results"), pattern = "*.\\_13367.rds"),
            list.files(here::here("results"), pattern = "*.\\_10770.rds"))

all_logistic =
  map_dfr(res_logistic,
          .f = function(x){
            read_rds(here::here("results", x)) %>%
              mutate(name = x) %>%
              mutate(fold = as.numeric(fold))
          }) %>%
  mutate(
    type = case_when(
      sub(".*\\d(.+).rds.*", "\\1", name) == "xgb" ~ "XGBoost",
      sub(".*\\d(.+).rds.*", "\\1", name) == "long" ~ "Long",
      sub(".*\\d(.+).rds.*", "\\1", name) == "lasso" ~ "Lasso",
      sub(".*\\d(.+).rds.*", "\\1", name) == "rf" ~ "Random Forest",
      sub(".*\\d(.+).rds.*", "\\1", name) == "fnl" ~ "Linear SoFR",
      sub(".*\\d(.+).rds.*", "\\1", name) == "nlfnl" ~ "Nonlinear SoFR",
      .default = "Logistic"
    ),
    paradigm = case_when(
      grepl("temporal2", name) ~ "temporal2",
      grepl("temporal", name) ~ "temporal",
      .default = "random"
    )
  ) %>%
  filter(paradigm != "temporal") %>%
  mutate(across(starts_with("rank"), ~.x / n * 100))

all_logistic %>%
  # filter(n %in% c(100, 500) & type != "Long") %>%
  group_by(paradigm, n) %>%
  mutate(n_groups = n()) %>%
  group_by(paradigm, n, n_groups) %>%
  summarize(across(starts_with("rank"), list(med = median, min = min, max = max)), .groups = "drop") %>%
  group_by(paradigm, n, n_groups) %>%
  arrange(n, .by_group = TRUE) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = glue('{rank1_med} [{rank1_min},{rank1_max}]'),
         r5 = glue('{rank5_med} [{rank5_min},{rank5_max}]'),
         r1pct = glue('{rank1pct_med} [{rank1pct_min},{rank1pct_max}]'),
         r5pct = glue('{rank5pct_med} [{rank5pct_min},{rank5pct_max}]')) %>%
  select(paradigm,  n, n_groups, r1, r1pct, r5, r5pct) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

## all logistic figure
p  = all_logistic %>%
  mutate(paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  group_by(n, paradigm) %>%
  summarize(across(starts_with("rank"), median)) %>%
  pivot_longer(cols = contains("rank"), names_to = "metric", values_to = "value") %>%
  # mutate(name = factor(name, labels = c("Rank 1", "Rank 5"))) %>%
  ggplot(aes(x = n, y = value, color = metric, group = metric)) +
  geom_jitter(width = 25, size = 2)  +
  geom_line(linewidth = 1.1) +
  facet_grid(.~paradigm, scales = "free_x") +
  scale_x_continuous(breaks=c(100, 500, 1000, 2500, 5000, 10000, 10770, 13367),
                     labels = c(1, 5, 10, 25, 50, 100, 108, 133)) +
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
                     labels = c("Rank 1", "Rank 1%", "Rank 5", "Rank 5%"), name = "Metric")
  # geom_text_repel(data = . %>%
  #                   filter(n_sub %in% c(100, 11225, 13367)),
  #                 aes(label = round(value, 0)), size = 2.5)
png(here::here("manuscript", "figs", "acc_pct.png"), width = 9, height = 6, res = 100, units = "in")
p
dev.off()


## long table

long_files = list.files(here::here("results"), pattern = "long")

all_long =
  map_dfr(long_files,
          .f = function(x){
            read_rds(here::here("results", x)) %>%
              mutate(name = x) %>%
              mutate(fold = as.numeric(fold))
          }) %>%
  mutate(
    type = case_when(
      sub(".*\\d(.+).rds.*", "\\1", name) == "xgb" ~ "XGBoost",
      sub(".*\\d(.+).rds.*", "\\1", name) == "long" ~ "Long",
      sub(".*\\d(.+).rds.*", "\\1", name) == "lasso" ~ "Lasso",
      sub(".*\\d(.+).rds.*", "\\1", name) == "rf" ~ "Random Forest",
      sub(".*\\d(.+).rds.*", "\\1", name) == "fnl" ~ "Linear SoFR",
      sub(".*\\d(.+).rds.*", "\\1", name) == "nlfnl" ~ "Nonlinear SoFR",
      .default = "Logistic"
    ),
    paradigm = case_when(
      grepl("temporal2", name) ~ "temporal2",
      grepl("temporal", name) ~ "temporal",
      .default = "random"
    )
  ) %>%
  filter(paradigm != "temporal") %>%
  mutate(across(starts_with("rank"), ~.x / n * 100))

all_long %>%
  mutate(long = "Long") %>%
  bind_rows(all_logistic %>% mutate(long = "Default")) %>%
  filter(n %in% c(100, 2500, 5000, 10129, 10000)) %>%
  mutate(paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  group_by(paradigm, long, n) %>%
  summarize(across(c(rank1, rank5), median), .groups = "drop") %>%
  pivot_longer(cols = starts_with("rank"), names_to = "metric") %>%
  ggplot(aes(x = n, y = value, color = long, linetype = metric, shape = metric)) +
  # geom_point(size = 1) +
  geom_line(linewidth = 1.1) +
  facet_wrap(.~paradigm) +
  scale_x_continuous(breaks=c(100,  1000, 2500, 5000, 10129),
                     labels = c(1, 10, 25, 50, 100)) +
  scale_y_continuous(breaks=seq(0,100, 10), limits = c(0,100)) +
  guides(color = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1)) +
  scale_linetype_discrete(name = "Metric", labels = c("Rank 1", "Rank 5")) +
  # scale_shape_discrete(name = "Metric", labels = c("Rank 1", "Rank 5")) +
  scale_color_manual(name = "Amount of Data", labels = c("3 minutes", "6 minutes"),
                       values = c("#924900FF", "#6DB6FFFF")) +
  labs(x = "Number of Subjects (x100)", y = "Accuracy")

## oversampling
ov_files = list.files(here::here("results"), pattern = "over", full.names = TRUE)

small = ov_files[c(2:4, 9, 10, 12)]

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
         factor = "default")

res_over %>%
  filter(factor < 1) %>%
  mutate(factor = as.character(factor)) %>%
  bind_rows(default) %>%
  pivot_longer(cols = c(rank1, rank5), names_to = "metric", values_to = "value") %>%
  mutate(ovsamp = factor(factor),
         metric = factor(metric, labels = c("Rank 1", "Rank 5")),
         paradigm = factor(paradigm, labels = c("Random", "Temporal"))) %>%
  ggplot(aes(x = n, y = value, color = ovsamp)) +
  geom_point(size = 2) +
  geom_line(aes(group = ovsamp), linewidth = .9) +
  facet_grid(paradigm~metric, scales = "free_y") +
  theme_light() +
  scale_color_manual(values = c("#290AD8FF", "#3FA0FFFF", "#FFE099FF",
                                "#FFAD72FF", "#F76D5EFF", "grey"), name = "Proportion of data\ncomprised by outcome")+
  labs(x = "Subset Size", y = "Median Accuracy") +
  scale_x_continuous(breaks=c(100, 500, 1000), labels = c(100, 500, 1000))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))



## table
small = ov_files[c(1, 11)]

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
         factor = "default")

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
            fill = "#D82632FF", alpha = 0.5) +
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
png(here::here("manuscript", "figs", "figure1_3panel.png"), width = 6, height = 8, units = "in",
    res = 100)
p1 / p2 / p3 + plot_annotation(tag_levels = "A")
dev.off()

p = day2 %>%
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
               color = "#290AD8FF", linewidth = 1.1) +
  annotate(geom = "label", x = as.POSIXct("2000-01-03 08:03:38", tz = "UTC"),
           y = 1.95,
           color = "#290AD8FF", label = "Bout 1, included", vjust = -1, size = 5) +
  annotate(geom = "segment", x = as.POSIXct("2000-01-03 08:03:48", tz = "UTC"),
           xend = as.POSIXct("2000-01-03 08:04:01", tz = "UTC"),
           y = 2.1,
           color = "#FFAD72FF", linewidth = 1.1) +
  annotate(geom = "label", x = as.POSIXct("2000-01-03 08:03:51", tz = "UTC"),
           y = 1.88, size = 5,
           color = "#FFAD72FF", label = "Bout 2, not included", vjust = -1)

png(here::here("manuscript", "figs", "walking_segments_v2.png"), width = 10, height = 6, units = "in", res = 300)
p
dev.off()

svg(here::here("manuscript", "figs", "walking_segments_v2.svg"), width = 10, height = 5)
p
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

png(here::here("manuscript", "figs", "p1_v2.png"), width = 6, height = 2.5, units = "in", res = 500)
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

png(here::here("manuscript", "figs", "p2_v2.png"), width = 6, height = 2.5, units = "in", res = 500)
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
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16)) +
  # axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(x = "Acceleration range (g)", y = "Lag acceleration range (g)")

png(here::here("manuscript", "figs", "p3_v2.png"), width = 6, height = 2.5, units = "in", res = 500)
p3
dev.off()

png(here::here("manuscript", "figs", "grid_cells_v2.png"), width = 10, height = 10, units = "in", res = 300)
p1 / p2 / p3
dev.off()
