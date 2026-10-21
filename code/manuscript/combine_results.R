library(tidyverse)
library(glue)
library(paletteer)
library(patchwork)
theme_set(theme_light(base_size = 12))
covars = read_rds(here::here("data", "covariates_accel_mortality_df.rds"))
covars_y = read_rds(here::here("data", "covariates_accel_df_paxy.rds"))
library(gt)

if(!dir.exists(here::here("results", "summarized"))) dir.create(here::here("results", "summarized"))

all_subs = read_rds(here::here("data", "fingerprint_folds.rds"))
temporal_subs = read_rds(here::here("data", "fingerprint_folds_temporal2.rds"))


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

all_100 = 
  all_100 |> 
  mutate(algorithm = if_else(grepl("scs", name), "stepcount", "adept"))

write_rds(all_100, here::here("results", "summarized", "all_n100.rds"))

res_500 = c(list.files(here::here("results"), pattern = "*.\\_500.rds"),
            list.files(here::here("results"), pattern = "*.\\_500fnl.rds"),
            list.files(here::here("results"), pattern = "*.\\_500nlfnl.rds"),
            list.files(here::here("results"), pattern = "*.\\_500lasso.rds"),
            list.files(here::here("results"), pattern = "*.\\_500xgb.rds"),
            list.files(here::here("results"), pattern = "*.\\_500rf.rds"))
res_500 = res_500[-grep("subsets", res_500)]



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

write_rds(all_500, here::here("results", "summarized", "all_n500.rds"))

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

write_rds(all_logistic, here::here("results", "summarized", "all_logistic.rds"))


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

write_rds(all_long, here::here("results", "summarized", "all_long.rds"))


## oversampling
ov_files = list.files(here::here("results"), pattern = "over", full.names = TRUE)
ov_files = ov_files[-grep("temporal\\_", ov_files)]


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
  res_over3$`1` |> 
  bind_rows(res_over3$`2`) |> 
  mutate(factor = c(0.1, 0.25))

res_over = bind_rows(res_over, res_over2, res_over3)

write_rds(res_over, here::here("results", "summarized", "all_oversample.rds"))

### table with n = N from all models 

logistic_random = read_rds(here::here("results", "prediction_res_13367.rds"))
wtd_random = read_rds(here::here("results", "prediction_res_13367wtd.rds"))
logistic_ov = read_rds(here::here("results", "prediction_res_13367over.rds"))
logistic_boost = read_rds(here::here("results", "prediction_res_13367boost.rds"))
logistic_wtdpca = read_rds(here::here("results", "prediction_res_wtdpca_13367.rds"))

logistic_temporal = read_rds(here::here("results", "prediction_res_temporal2_10770.rds"))
wtd_temporal = read_rds(here::here("results", "prediction_res_temporal2_10770wtd.rds"))
logistic_ov_temporal = read_rds(here::here("results", "prediction_res_temporal2_10770over.rds"))
logistic_boost_temporal = read_rds(here::here("results", "prediction_res_temporal2_10770boost.rds"))


big_n_df = 
  logistic_random |> 
  mutate(type = "Logistic", paradigm = "Random") |> 
  bind_rows(wtd_random |> mutate(type = "Weighted Logistic", paradigm = "Random")) |> 
  bind_rows(logistic_ov |> bind_rows() |> slice(1) |> mutate(type = "Logistic Oversampled", paradigm = "Random")) |> 
  bind_rows(logistic_wtdpca |> mutate(type = "Weighted Logistic PCA", paradigm = "Random")) |> 
  bind_rows(logistic_boost |> select(-rank) |> mutate(type = "Logistic Boost", paradigm = "Random")) |> 
  bind_rows(logistic_temporal |> mutate(type = "Logistic", paradigm = "Temporal")) |> 
  bind_rows(wtd_temporal |> mutate(type = "Weighted Logistic", paradigm = "Temporal")) |> 
  bind_rows(logistic_ov_temporal$`1` |> mutate(type = "Logistic Oversampled", paradigm = "Temporal")) |> 
  bind_rows(logistic_boost_temporal |> select(-rank) |>  mutate(type = "Logistic Boost", paradigm = "Temporal")) |> 
  mutate(across(contains("rank"), ~.x / n * 100)) 

write_rds(big_n_df, here::here("results", "summarized", "full_sample_models.rds"))


### revision results with stepcount 

if(!file.exists(here::here("results", "summarized", "all_revision_results.rds"))){
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
                                      "prediction_res_temporal2_100.rds")) %>%
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
  
  write_rds(all_res, here::here("results", "summarized", "all_revision_results.rds"))
  
} else {
  all_res = read_rds(here::here("results", "summarized", "all_revision_results.rds"))
}

### subset training results 

subsets100 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_100_v5.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 100)


subsets500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_500_v5.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 500)

subsets1000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_1000_v5.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 1000)


subsets2500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_2500_v5.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 2500)


subsets5000 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_5000_v5.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 5000)


subsets7500 = read_rds(here::here("results", "prediction_res_subsets_13367wtd_7500_v5.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 7500)

all = read_rds(here::here("results", "prediction_res_13367wtd.rds")) %>%
  select(-n_target) |> 
  mutate(across(-n, ~.x / n * 100)) %>%
  mutate(train_size = 13367)

comb =
  subsets100 %>%
  bind_rows(subsets500, subsets1000, subsets2500, subsets5000, subsets7500, all) %>%
  select(-fold)


write_rds(comb, here::here("results", "summarized", "subset_train_results.rds"))

### SIMEX results 
mixed = read_rds(here::here("results", "prediction_res_mixedwtd_13367.rds"))
random = read_rds(here::here("results", "prediction_res_randomwtd_13367.rds"))
default = read_rds(here::here("results", "prediction_res_13367wtd.rds"))
sc = read_rds(here::here("results", "prediction_res_scs_13367wtd.rds"))

simex = 
  random |> mutate(type = "random") |> 
  bind_rows(mixed|> mutate(type = "mixed")) |> 
  bind_rows(sc |> mutate(type = "sc")) |> 
  bind_rows(default |> mutate(type = "adept")) 

write_rds(simex, here::here("results", "summarized", "simex_exp.rds"))

### benchmarking results 
bench = read_rds(here::here("results", "bench_adept.rds"))
bench =
  bench %>%
  rename(train_size = test) %>%
  mutate(train_size = if_else(train_size == "N", "13367", train_size),
         train_size = as.numeric(train_size))

write_rds(bench, here::here("results", "summarized", "benchmark_subsets.rds"))
