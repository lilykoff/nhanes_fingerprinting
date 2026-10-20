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
res_500 = res_500[-grep("subsets", res_500)]


temp = read_rds(here::here("results", "prediction_res_500rf.rds"))

temp %>%
  mutate(r1 = rank1 / n * 100) %>%
  summarize(r = median(r1))

temp = read_rds(here::here("results", "prediction_res_100rf.rds"))
temp %>%
  mutate(r1 = rank1 / n * 100) %>%
  summarize(r = median(r1))

temp %>%
  mutate(r1 = rank1 / n * 100) %>%
  summarize(r = median(r1))
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

write_rds(all_500, here::here("results", "all_n500.rds"))

res_logistic = c(list.files(here::here("results"), pattern = "*.\\_100.rds"),
                 list.files(here::here("results"), pattern = "*.\\_500.rds"),
                 list.files(here::here("results"), pattern = "*.\\_1000.rds"),
                 list.files(here::here("results"), pattern = "*.\\_2500.rds"),
                 list.files(here::here("results"), pattern = "*.\\_5000.rds"),
                 list.files(here::here("results"), pattern = "*.\\_10000.rds"),
                 list.files(here::here("results"), pattern = "*.\\_13367.rds"),
                 list.files(here::here("results"), pattern = "*.\\_10770.rds"))

res_logistic = res_logistic[-grep("sc", res_logistic)]
res_logistic = res_logistic[-grep("subset", res_logistic)]
res_logistic = res_logistic[-grep("wtd", res_logistic)]

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

write_rds(all_logistic, here::here("results", "all_logistic.rds"))


## long table

long_files = list.files(here::here("results"), pattern = "long")
long_files = long_files[!grepl("subset", long_files)]

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

write_rds(all_long, here::here("results", "all_long.rds"))


## oversampling
ov_files = list.files(here::here("results"), pattern = "over", full.names = TRUE)
# ov_files = ov_files[-c(1, 4, 9)]
# small = ov_files[c(2:3, 5, 10, 11, 14)]

res_over = map_dfr(ov_files, .f = function(x){
  tmp = read_rds(x)
  if(is.data.frame(tmp)){
    return(tmp %>% mutate(name = x))
  } else{
    return(NULL)
  }}) %>%
  ungroup() %>%
  mutate(paradigm = if_else(grepl("temporal", name), "temporal", "random"),
         across(contains("rank"), ~.x / n)) %>%
  group_by(paradigm, n, factor) %>%
  summarize(across(contains("rank"), ~median(.x) * 100),
            .groups = "drop")


res_over2 = read_rds(here::here("results/prediction_res_13367over.rds")) %>%
  bind_rows() %>%
  mutate(factor = c(0.1, 0.25))

res_over3 = read_rds(here::here("results/prediction_res_temporal2_10770over.rds"))
res_over3 =
  tibble(
    rank1 = c(464, 313),
    rank5 = c(1120, 848),
    rank1pct = c(3400, 3133),
    rank5pct = c(5584, 5369),
    n = c(10770, 10770),
    fold = c(1, 1),
    factor = c(0.1, 0.25))

res_over = bind_rows(res_over, res_over2, res_over3)

write_rds(res_over, here::here("results", "all_oversample.rds"))


## weighting table
weight_files = list.files(here::here("results"), pattern = "wtd", full.names = TRUE)
weight_files = weight_files[-grep("subsets", weight_files)]
weight_files = weight_files[-grep("sc", weight_files)]
weight_files = weight_files[-grep("pca", weight_files)]
weight_files = weight_files[-grep("mixed", weight_files)]
weight_files = weight_files[-grep("random", weight_files)]

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

two_stage = read_rds(here::here("results", "prediction_res_13367boost.rds"))
two_stage  = two_stage %>% mutate(type = "Two Stage", paradigm = "Random")

two_stage2 = read_rds(here::here("results", "prediction_res_temporal2_10770boost.rds"))
two_stage2 = two_stage2 %>% mutate(type = "Two Stage", paradigm = "Temporal")


extra =
  all_wt %>%
  bind_rows(all_logistic_lg) %>%
  bind_rows(ovsamp) %>%
  bind_rows(long_lg)

write_rds(extra, here::here("results", "notable_res.rds"))

# ## two stage models
# two_stage = read_rds(here::here("results", "prediction_res_13367boost.rds"))
# two_stage
#
# two_stage2 = read_rds(here::here("results", "prediction_res_temporal_10770boost.rds"))
# two_stage2
# # bouts

