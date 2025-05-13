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
            list.files(here::here("results"), pattern = "*.\\_500lasso.rds"),
            list.files(here::here("results"), pattern = "*.\\_500xgb.rds"),
            list.files(here::here("results"), pattern = "*.\\_500rf.rds"))

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
  select(contains("pct"), type, paradigm) %>%
  group_by(type, paradigm) %>%
  summarize(across(contains("pct"), median), .groups = "drop") %>%
  pivot_wider(names_from = paradigm, values_from = contains("pct"))

all_500 %>%
  select(contains("pct"), type, paradigm) %>%
  group_by(type, paradigm) %>%
  summarize(across(contains("pct"), median), .groups = "drop") %>%
  pivot_wider(names_from = paradigm, values_from = contains("pct"))

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

all_100 %>%
  bind_rows(all_500 %>% mutate(fold = as.character(fold))) %>%
  mutate(across(contains("rank"), ~(.x / n)*100)) %>%
  # filter(n %in% c(100, 500) & type != "Long") %>%
  group_by(paradigm, type, n) %>%
  summarize(across(contains("rank"), list(med = median, min = min, max = max)), .groups = "drop") %>%
  group_by(paradigm) %>%
  arrange(desc(rank1_med), .by_group = TRUE) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = glue('{rank1_med} [{rank1_min},{rank1_max}]'),
         r5 = glue('{rank5_med} [{rank5_min},{rank5_max}]'),
         r1p = glue('{rank1pct_med} [{rank1pct_min},{rank1pct_max}]'),
         r5p = glue('{rank5pct_med} [{rank5pct_min},{rank5pct_max}]')) %>%
  select(paradigm, type, n, r1, r5, r1p, r5p) %>%
  pivot_wider(names_from = n, values_from = c(r1, r5, r1p, r5p)) %>%
  select(-r1p_100, -r5p_100, -r1p_500) %>%
  select(paradigm, type, contains("p")) %>%
  kableExtra::kbl("latex", booktabs = TRUE)



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



## long table

long_files = list.files(here::here("results"), pattern = "long")
# long_files = long_files[!grepl("subset", long_files)]

all_long =
  map_dfr(long_files,
          .f = function(x){
            read_rds(here::here("results", x)) %>%
              mutate(name = x) %>%
              mutate(fold = as.numeric(fold))
          }) %>%
  mutate(type =
           if_else(grepl("subset", name), "Regular", "Long"),
         paradigm = if_else(grepl("temporal", name), "Temporal", "Random")) %>%
  mutate(across(contains("rank"), ~(.x / n) * 100)) %>%
  mutate(n = if_else(n == 7929, 8018, n)) %>%
  group_by(name, n, type, paradigm) %>%
  summarize(across(contains("rank"), median), .groups = "drop") %>%
  select(-name)

reg = all_logistic %>%
  group_by(n, type, paradigm) %>%
  summarize(across(contains("rank"), median),.groups = "drop") %>%
  filter(n %in% c(100, 2500, 5000)) %>%
  mutate(type = "Regular") %>%
  mutate(paradigm = if_else(paradigm == "random", "Random", "Temporal"))

comb =
  all_long %>%
  filter(n != 1000) %>%
  bind_rows(reg)


comb %>%
  pivot_wider(names_from = type, values_from = c(rank1, rank5, rank1pct, rank5pct)) %>%
  arrange(n) %>%
  arrange(paradigm) %>%
  select(paradigm, n, rank1_Regular, rank1_Long, rank5_Regular, rank5_Long,
         rank1pct_Regular, rank1pct_Long, rank5pct_Regular, rank5pct_Long) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

long_files = list.files(here::here("results"), pattern = "long")
long_files = c(here::here("results", "prediction_res_10129long.rds"),
               here::here("results", "prediction_res_10129longsubset.rds"),
               here::here("results", "prediction_res_temporal_8018long.rds"),
               here::here("results", "prediction_res_temporal_8018longsubset.rds"))
all_long =
  map_dfr(long_files,
          .f = function(x){
            read_rds(x) %>%
              mutate(name = x) %>%
              mutate(fold = as.numeric(fold))
          })

## oversampling
ov_files = list.files(here::here("results"), pattern = "over", full.names = TRUE)

small = ov_files[c(2:3, 5, 10, 11, 14)]

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




## table
small = ov_files[c(4, 12)]
read_rds(small[1]) %>%
  bind_rows() %>%
  mutate(across(starts_with("rank"), ~.x/n * 100)) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2))))



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


## weighting table
weight_files = list.files(here::here("results"), pattern = "wtd", full.names = TRUE)
weight_files

all_wt = map_dfr(weight_files, read_rds) %>%
  mutate(across(contains("rank"), ~.x / n * 100)) %>%
  mutate(type = "Weighted",
         paradigm = c("Random", "Temporal"))

all_logistic_lg =
  all_logistic %>%
  filter(n %in% c(13367, 10770)) %>%
  mutate(paradigm = c("Random", "Temporal"))

all_ovsamp = read_rds(here::here("results", "prediction_res_temporal2_10770over.rds"))

ovsamp =
  tibble(rank1 = 464,
         rank5 = 1120,
         rank1pct = 3400,
         rank5pct = 5584,
         n = 10770) %>%
  mutate(paradigm = "Temporal",
         type = "Oversampled") %>%
  mutate(across(contains("rank"), ~.x / n * 100))

long_lg = map_dfr(.x = c(here::here("results",
                                    "prediction_res_temporal_8018long.rds"),
                         here::here("results",
                                    "prediction_res_10129long.rds")),
                  .f = read_rds) %>%
  mutate(paradigm = c("Temporal", "Random"),
         type = "Long") %>%
  mutate(across(contains("rank"), ~.x / n * 100))


all_wt %>%
  bind_rows(all_logistic_lg) %>%
  bind_rows(ovsamp) %>%
  bind_rows(long_lg) %>%
  select(contains("rank"), n, type, paradigm) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  group_by(paradigm) %>%
  # arrange(paradigm) %>%
  arrange(rank1, .by_group = TRUE) %>%
  select(paradigm, type, n, starts_with("rank1"), everything()) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

## two stage models
two_stage = read_rds(here::here("results", "prediction_res_13367boost.rds"))
two_stage

# bouts

bouts = read_csv(here::here("data", "walking_segments.csv.gz"))
bouts %>%
  group_by(id) %>%
  count() %>%
  ungroup() %>%
  summarize(across(n, ~quantile(.x, c(0.25, .5, .75))))
bouts %>%
  summarize(across(n_seconds, ~quantile(.x, c(0.25, .5, .75))))

