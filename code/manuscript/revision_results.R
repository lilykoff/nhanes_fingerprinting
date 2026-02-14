library(tidyverse)
library(glue)
library(paletteer)
library(patchwork)
library(gtsummary)
theme_set(theme_light(base_size = 12))
covars = read_rds(here::here("data", "covariates_accel_mortality_df.rds"))
covars_y = read_rds(here::here("data", "covariates_accel_df_paxy.rds"))
library(gt)

## --- comparison of ADEPT and SC ---- ##
sc_30m_100 = read_rds(here::here("results",
                              "prediction_res_sc_100.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "30_min",
         type = "unweighted",
         paradigm = "random")
sc_30m_100_t = read_rds(here::here("results",
                               "prediction_res_temporal_sc_100.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "30_min",
         type = "unweighted",
         paradigm = "temporal")

sc_3min_100 = read_rds(here::here("results",
                        "prediction_res_scs_100.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "3_min",
         type = "unweighted",
         paradigm = "random")
sc_3min_100_t = read_rds(here::here("results",
                         "prediction_res_temporal_scs_100.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "3_min",
         type = "unweighted",
         paradigm = "temporal")

ad_30min_100 = read_rds(here::here("results", "prediction_res_100_30min.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "30_min",
         type = "unweighted",
         paradigm = "random") %>%
  mutate(fold = as.character(fold))


ad_30min_100_t = read_rds(here::here("results", "prediction_res_temporal_100_30min.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "30_min",
         type = "unweighted",
         paradigm = "temporal") %>%
  mutate(fold = as.character(fold))

ad_30min_1541_t = read_rds(here::here("results", "prediction_res_temporal_30min.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "30_min",
         type = "unweighted",
         paradigm = "temporal") %>%
  mutate(fold = as.character(fold))

sc_30min_1541_t = read_rds(here::here("results", "prediction_res_temporal_30min_sc.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "30_min",
         type = "unweighted",
         paradigm = "temporal") %>%
  mutate(fold = as.character(fold))

ad_30min_5302 = read_rds(here::here("results", "prediction_res_30min.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "30_min",
         type = "unweighted",
         paradigm = "random") %>%
  mutate(fold = as.character(fold))

sc_30min_5302 = read_rds(here::here("results", "prediction_res_30min_sc.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "30_min",
         type = "unweighted",
         paradigm = "random") %>%
  mutate(fold = as.character(fold))


ad_3min_100 = read_rds(here::here("results",
                             "prediction_res_100.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "3_min",
         type = "unweighted",
         paradigm = "random")

ad_3min_100_t = read_rds(here::here("results",
                              "prediction_res_temporal_100.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "3_min",
         type = "unweighted",
         paradigm = "temporal")

ad_3min_13367 = read_rds(here::here("results", "prediction_res_13367.rds")) %>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "3_min",
         type = "unweighted",
         paradigm = "random")
ad_3min_13367w = read_rds(here::here("results", "prediction_res_13367wtd.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "3_min",
         type = "weighted",
         paradigm = "random")

sc_3min_13367 = read_rds(here::here("results", "prediction_res_scs_13367.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "3_min",
         type = "unweighted",
         paradigm = "random")


sc_3min_13367w = read_rds(here::here("results", "prediction_res_scs_13367wtd.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "3_min",
         type = "weighted",
         paradigm = "random")

ad_3min_10770_t = read_rds(here::here("results", "prediction_res_temporal2_10770.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "3_min",
         type = "unweighted",
         paradigm = "temporal")

ad_3min_10770w_t = read_rds(here::here("results", "prediction_res_temporal2_10770wtd.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "adept",
         time = "3_min",
         type = "weighted",
         paradigm = "temporal")

sc_3min_10770_t = read_rds(here::here("results", "prediction_res_temporal_scs_10770.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "3_min",
         type = "unweighted",
         paradigm = "temporal")
sc_3min_10770w_t = read_rds(here::here("results", "prediction_res_temporal_scs_10770wtd.rds"))%>%
  mutate(fold = as.character(fold)) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "3_min",
         type = "weighted",
         paradigm = "temporal")

sc_30min_15374w_t = read_rds(here::here("results", "prediction_res_temporal_sc_15374wtd.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "30_min",
         type = "weighted",
         paradigm = "temporal") %>%
  mutate(fold = as.character(fold))

sc_30min_15374w = read_rds(here::here("results", "prediction_res_sc_15374wtd.rds")) %>%
  mutate(across(starts_with("rank"), ~.x / n * 100),
         algorithm = "stepcount",
         time = "30_min",
         type = "weighted",
         paradigm = "random") %>%
  mutate(fold = as.character(fold))
rf = read_rds(here::here("results", "prediction_res_1000rf.rds"))


# make big results file by binding everything together

all_res = bind_rows(sc_30m_100,
                    sc_30m_100_t,
                    sc_3min_100,
                    sc_3min_100_t,
                    ad_30min_100,
                    ad_30min_100_t,
                    ad_30min_1541_t,
                    sc_30min_1541_t,
                    ad_30min_5302,
                    sc_30min_5302,
                    ad_3min_100,
                    ad_3min_100_t,
                    ad_3min_13367,
                    ad_3min_13367w,
                    sc_3min_13367,
                    sc_3min_13367w,
                    ad_3min_10770_t,
                    ad_3min_10770w_t,
                    sc_3min_10770_t,
                    sc_3min_10770w_t,
                    sc_30min_15374w_t,
                    sc_30min_15374w) %>%
  select(-fold, -n_tar, -n_target)


all_res


## all results using 3 min of data

all_res_3min =
  all_res %>%
  filter(time == "3_min") %>%
  group_by(algorithm, time, paradigm, n, type) %>%
  summarize(across(c(rank1, rank5), list(med = median, min = min, max = max)), .groups = "drop")

## all results using 30 min of data

all_res_30min =
  all_res %>%
  filter(time == "30_min") %>%
  group_by(algorithm, time, paradigm, n, type) %>%
  summarize(across(c(rank1, rank5), list(med = median, min = min, max = max)), .groups = "drop")


# table for 3 min of data
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


# table for 30 min of data
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


## rf processing
rf %>%
  mutate(across(starts_with("rank"), ~.x / n * 100)) %>%
  summarize(across(starts_with("rank"), list(min = min, max = max, med = median)))

rf %>%
  mutate(across(starts_with("rank"), ~.x / n * 100)) %>%
  summarize(across(starts_with("rank"), list(min = min, max = max, med = median))) %>%
  t()



all_subs = read_rds(here::here("data", "fingerprint_folds.rds"))
temporal_subs = read_rds(here::here("data", "fingerprint_folds_temporal2.rds"))
sc_subs = read_rds(here::here("data", "folds_sc.rds"))
### population summaries
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
