#### generate all tables library(tidyverse)
library(glue)
library(paletteer)
library(patchwork)
theme_set(theme_light(base_size = 12))
library(gt)


####################
# READ IN DATA 
####################
all_logistic = read_rds(here::here("results", "summarized", "all_logistic.rds"))
all_100 = read_rds(here::here("results", "summarized", "all_n100.rds"))
all_500 = read_rds(here::here("results", "summarized", "all_n500.rds"))
res_1000rf = read_rds(here::here("results", "summarized", "prediction_res_1000rf.rds"))
all_big_n = read_rds(here::here("results", "summarized", "full_sample_models.rds"))
all_res = read_rds(here::here("results", "summarized", "all_revision_results.rds"))
simex = read_rds(here::here("results", "summarized", "simex_exp.rds"))

all_subs = read_rds(here::here("data", "fingerprint_folds.rds"))
temporal_subs = read_rds(here::here("data", "fingerprint_folds_temporal2.rds"))
sc_subs = read_rds(here::here("data", "folds_sc.rds"))

covars = read_rds(here::here("data", "covariates_accel_mortality_df.rds"))
covars_y = read_rds(here::here("data", "covariates_accel_df_paxy.rds"))


####################
# TABLE ONE: CHARACTERISTICS OF THE SAMPLE ## 
####################

df =
  covars %>%
  bind_rows(covars_y)  %>%
  filter(has_accel) %>%
  mutate(data_release_cycle = if_else(is.na(data_release_cycle), "NYFFS", as.character(data_release_cycle))) %>%
  mutate(random = SEQN %in% all_subs$id,
         temporal = SEQN %in% temporal_subs$id,
         sc = SEQN %in% sc_subs$id)


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

t4 =
  df %>%
  filter(sc) %>%
  select(data_release_cycle, gender, age = age_in_years_at_screening,
         race = race_hispanic_origin, weight_kg, standing_height_cm, body_mass_index_kg_m_2) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Stepcount**")

tbl_merge(
  tbls = list(t1, t2, t3, t4),
  tab_spanner = c("**All participants with acceleromtery**", "**Random Paradigm**", "**Temporal Paradigm**", "**Stepcount algorithm**")
) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

####################
## TABLE TWO: RANK 1, RANK 5 ACCURACIES FOR N=100, N=500
####################

all_100 %>%
  filter(algorithm == "adept") |> 
  bind_rows(all_500 %>% mutate(fold = as.character(fold))) %>%
  select(contains("rank"), n, type, paradigm) |> 
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
  # select(paradigm, type, contains("p")) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

####################
## TABLE THREE: LOGISTIC REGRESSION MODELS COMPARISON
####################



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

# random forest, for adding to table 
res_1000rf |> 
  mutate(across(contains("rank"), ~.x / n * 100)) |> 
  summarize(across(starts_with("rank"), list(med = median, min = min, max = max)), .groups = "drop") %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = glue('{rank1_med} [{rank1_min},{rank1_max}]'),
         r5 = glue('{rank5_med} [{rank5_min},{rank5_max}]'),
         r1pct = glue('{rank1pct_med} [{rank1pct_min},{rank1pct_max}]'),
         r5pct = glue('{rank5pct_med} [{rank5pct_min},{rank5pct_max}]')) |> 
  select( r1, r1pct, r5, r5pct) 


####################
## TABLE FOUR: BEST MODELS ON ENTIRE POPULATION 
####################

all_big_n |> 
  select(type, paradigm, rank1, rank1pct, rank5, rank5pct) |> 
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

####################
## TABLE FIVE: STEPCOUNT VS ADEPT TABLE 
####################


all_res_3min =
  all_res %>%
  filter(time == "3_min") %>%
  group_by(algorithm, time, paradigm, n, type) %>%
  summarize(across(c(rank1, rank5), list(med = median, min = min, max = max)), .groups = "drop")


all_res_30min =
  all_res %>%
  filter(time == "30_min") %>%
  group_by(algorithm, time, paradigm, n, type) %>%
  summarize(across(c(rank1, rank5), list(med = median, min = min, max = max)), .groups = "drop")

# table 5; 3 min 
all_res_3min %>%
  mutate(n = if_else(n == 13363, 13367, n),
         n = if_else(n == 10768, 10770, n)) %>% # fixing small n discrepancies
  group_by(paradigm) %>%
  arrange(desc(rank1_med), .by_group = TRUE) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = if_else(n == 100, glue('{rank1_med} [{rank1_min},{rank1_max}]'), glue('{rank1_med}')),
         r5 = if_else(n == 100, glue('{rank5_med} [{rank5_min},{rank5_max}]'), glue ('{rank5_med}'))) %>%
  select(paradigm, type, algorithm, n, r1, r5) %>%
  pivot_wider(names_from = algorithm, values_from = c(r1, r5))  %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# table 5; 30 min 
all_res_30min %>%
  mutate(n = if_else(n == 13363, 13367, n),
         n = if_else(n == 10768, 10770, n),
         n = if_else(n == 1530, 1541, n),
         n = if_else(n == 5237, 5302, n)) %>% # fixing small n discrepancies
  group_by(paradigm) %>%
  arrange(desc(rank1_med), .by_group = TRUE) %>%
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) %>%
  mutate(r1 = if_else(n == 100, glue('{rank1_med} [{rank1_min},{rank1_max}]'), glue('{rank1_med}')),
         r5 = if_else(n == 100, glue('{rank5_med} [{rank5_min},{rank5_max}]'), glue ('{rank5_med}'))) %>%
  select(paradigm, type, algorithm, n, r1, r5) %>%
  pivot_wider(names_from = algorithm, values_from = c(r1, r5))  %>%
  kableExtra::kbl("latex", booktabs = TRUE)

####################
## TABLE SIX: SIMEX TABLE 
####################

simex |> 
  mutate(across(contains("rank"), ~.x / n * 100)) |> 
  select(type, rank1, rank1pct, rank5, rank5pct) |> 
  mutate(across(contains("rank"), ~sprintf("%.2g", signif(.x, 2)))) |> 
  kableExtra::kbl("latex", booktabs = TRUE)



###### random - summarize walking bouts 
bouts = read_csv(here::here("data", "walking_segments.csv.gz"))
bouts %>%
  group_by(id) %>%
  count() %>%
  ungroup() %>%
  summarize(across(n, ~quantile(.x, c(0.25, .5, .75))))
bouts %>%
  summarize(across(n_seconds, ~quantile(.x, c(0.25, .5, .75))))
